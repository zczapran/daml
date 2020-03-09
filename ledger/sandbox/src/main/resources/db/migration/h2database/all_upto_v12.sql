-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- PART 1: copied and adapted from postgres/V1__Init.sql

-- Stores the history of the ledger -- mostly transactions. This table
-- is immutable in the sense that rows can never be modified, only
-- added.
CREATE TABLE ledger_entries
(
  -- Every entry is indexed by a monotonically growing integer. That is,
  -- new rows can only have a ledger_offet which is greater than the
  -- larger ledger_offset in ledger_entries. However, note that there
  -- might be gaps in the series formed by all the ledger_offsets in the
  -- table.
  ledger_offset         bigint primary key           not null,
  -- one of 'transaction', 'rejection', or 'checkpoint' -- also see
  -- check_entry below. note that we _could_ store the different entries
  -- in different tables, but as of now we deem more convient having a
  -- single table even if it imposes the constraint below, since this
  -- table represents a single unified stream of events and partitioning
  -- it across tables would be more inconvient. We might revise this in
  -- the future.
  typ                   varchar                      not null,
  -- see ledger API definition for more infos on some of these these fields
  transaction_id        varchar unique,
  command_id            varchar,
  application_id        varchar,
  submitter             varchar,
  workflow_id           varchar,
  effective_at          timestamp, -- with time zone
  recorded_at           timestamp                    not null, -- with time zone
  -- The transaction is stored using the .proto definition in
  -- `daml-lf/transaction/src/main/protobuf/com/digitalasset/daml/lf/transaction.proto`, and
  -- encoded using
  -- `daml-lf/transaction/src/main/protobuf/com/digitalasset/daml/lf/transaction.proto`.
  transaction           bytea,
  rejection_type        varchar,
  rejection_description varchar,

  -- note that this is not supposed to be a complete check, for example we do not check
  -- that fields that are not supposed to be present are indeed null.
  -- Reflects fix from V10__Loosen_transaction_check.sql
  constraint check_entry
    CHECK (
        (typ = 'transaction' and transaction_id is not null and effective_at is not null and transaction is not null and
         (
             (submitter is null and application_id is null and command_id is null) or
             (submitter is not null and application_id is not null and command_id is not null)
           )) or
        (typ = 'rejection' and command_id is not null and application_id is not null and submitter is not null and
         rejection_type is not null and rejection_description is not null) or
        (typ = 'checkpoint')
      )
);

-- This embodies the deduplication in the Ledger API.
-- Updated to V2__Command_deduplication.sql
CREATE UNIQUE INDEX idx_transactions_deduplication
  ON ledger_entries (submitter, command_id, application_id);

CREATE TABLE disclosures (
  transaction_id varchar not null,
  event_id       varchar not null,
  party          varchar not null,
  foreign key (transaction_id) references ledger_entries (transaction_id)
);

-- This schema version adds an index to the disclosures table
-- Added with V6__Disclosures_index.sql
CREATE INDEX idx_disclosures_transaction_id
  ON disclosures (transaction_id);

-- Note that technically this information is all present in `ledger_entries`,
-- but we store it in this form since it would not be viable to traverse
-- the entries every time we need to gain information as a contract. It's essentially
-- a materialized view of the contracts state.
CREATE TABLE contracts (
  id             varchar primary key not null,
  -- this is the transaction id that _originated_ the contract.
  transaction_id varchar             not null,
  -- this is the workflow id of the transaction above. note that this is
  -- a denormalization -- we could simply look up in the transaction table.
  -- we cache it here since we do not want to risk impacting performance
  -- by looking it up in `ledger_entries`, however we should verify this
  -- claim.
  workflow_id    varchar,
  -- This tuple is currently included in `contract`, since we encode
  -- the full value including all the identifiers. However we plan to
  -- move to a more compact representation that would need a pointer to
  -- the "top level" value type, and therefore we store the identifier
  -- here separately.
  package_id     varchar             not null,
  -- using the QualifiedName#toString format
  name           varchar             not null,
  -- this is denormalized much like `transaction_id` -- see comment above.
  create_offset  bigint              not null,
  -- this on the other hand _cannot_ be easily found out by looking into
  -- `ledger_entries` -- you'd have to traverse from `create_offset` which
  -- would be prohibitively expensive.
  archive_offset bigint,
  -- the serialized contract value, using the definition in
  -- `daml-lf/transaction/src/main/protobuf/com/digitalasset/daml/lf/value.proto`
  -- and the encoder in `ContractSerializer.scala`.
  -- Removed with V3__Contract_Divulgence.sql
  -- contract       bytea               not null,
  -- only present in contracts for templates that have a contract key definition.
  -- encoded using the definition in
  -- `daml-lf/transaction/src/main/protobuf/com/digitalasset/daml/lf/value.proto`.
  key            bytea,

  -- Added with v5_0__Extract_Event_Data.sql and V5_2__Extract_Event_data.sql
  create_event_id varchar            not null,

  foreign key (transaction_id) references ledger_entries (transaction_id),
  foreign key (create_offset) references ledger_entries (ledger_offset),
  foreign key (archive_offset) references ledger_entries (ledger_offset)
);

-- These two indices below could be a source performance bottleneck. Every additional index slows
-- down insertion. The contracts table will grow endlessly and the sole purpose of these indices is
-- to make ACS queries performant, while sacrificing insertion speed.
CREATE INDEX idx_contract_create_offset
  ON contracts (create_offset);
CREATE INDEX idx_contract_archive_offset
  ON contracts (archive_offset);

-- TODO what's the difference between this and `diclosures`? If we can rely on `event_id`
-- being the `contract_id`, isn't `disclosures` enough?
CREATE TABLE contract_witnesses (
  contract_id varchar not null,
  witness     varchar not null,
  foreign key (contract_id) references contracts (id)
);
CREATE UNIQUE INDEX contract_witnesses_idx
  ON contract_witnesses (contract_id, witness);

CREATE TABLE contract_key_maintainers (
  contract_id varchar not null,
  maintainer  varchar not null,
  foreign key (contract_id) references contracts (id)
);

CREATE UNIQUE INDEX contract_key_maintainers_idx
  ON contract_key_maintainers (contract_id, maintainer);

-- this table is meant to have a single row storing all the parameters we have
CREATE TABLE parameters (
  -- the generated or configured id identifying the ledger
  ledger_id varchar not null,
  -- stores the head offset, meant to change with every new ledger entry
  ledger_end bigint not null,
  -- the external ledger offset that corresponds to the index ledger end (added in Postgres V6)
  external_ledger_end varchar,
  -- Add the current configuration column to parameters.
  -- with V7__Add_configuration.sql
  configuration bytea
);

-- table to store a mapping from (template_id, contract value) to contract_id
-- contract values are binary blobs of unbounded size, the table therefore only stores a hash of the value
-- and relies for the hash to be collision free
CREATE TABLE contract_keys (
  package_id   varchar not null,
  -- using the QualifiedName#toString format
  name         varchar not null,
  -- stable SHA256 of the protobuf serialized key value, produced using
  -- `KeyHasher.scala`.
  value_hash   varchar not null,
  contract_id  varchar not null,
  PRIMARY KEY (package_id, name, value_hash),
  foreign key (contract_id) references contracts (id)
);

-- PART 2: copied and adapted from postgres/V2_0__Contract_divulgence.sql

---------------------------------------------------------------------------------------------------
-- V2: Contract divulgence
--
-- This schema version adds a table for tracking contract divulgence.
-- This is required for making sure contracts can only be fetched by parties that see the contract.
---------------------------------------------------------------------------------------------------


---------------------------------------------------------------------------------------------------
-- V4: Contract divulgence
--
-- This schema version builds on V1 and V3 by modifying the contract_divulgences.contract_id foreign key
-- to point to contract_data.id (rather than contracts.id previously). Because there is no way to alter a foreign key
-- constraint or to drop and add an unnamed constraint, the script rebuilds the contract_divulgences table.
-- Replaced with V4__Contract_Divulgence.sql
CREATE TABLE contract_divulgences (
  contract_id   varchar  not null,
  -- The party to which the given contract was divulged
  party         varchar  not null,
  -- The offset at which the contract was divulged to the given party
  ledger_offset bigint   not null,
  -- The transaction ID at which the contract was divulged to the given party
  transaction_id varchar not null,

  foreign key (contract_id) references contract_data (id), -- refer to contract_data instead, the reason for this script
  foreign key (ledger_offset) references ledger_entries (ledger_offset),
  foreign key (transaction_id) references ledger_entries (transaction_id),

  CONSTRAINT contract_divulgences_idx UNIQUE(contract_id, party)
);


-- PART 3: n/a scala-migration only, not applicable to newly introduced database types

-- PART 4: adopted unmodified from postgres/V4_0__Add_parties.sql

---------------------------------------------------------------------------------------------------
-- V4: List of parties
--
-- This schema version adds a table for tracking known parties.
-- In the sandbox, parties are added implicitly when they are first mentioned in a transaction,
-- or explicitly through an API call.
---------------------------------------------------------------------------------------------------



CREATE TABLE parties (
  -- The unique identifier of the party
  party varchar primary key not null,
  -- A human readable name of the party, might not be unique
  display_name varchar,
  -- True iff the party was added explicitly through an API call
  explicit bool not null,
  -- For implicitly added parties: the offset of the transaction that introduced the party
  -- For explicitly added parties: the ledger end at the time when the party was added
  ledger_offset bigint
);


-- PART 5: copied and adapted from postgres/V5__Add_packages.sql

---------------------------------------------------------------------------------------------------
-- V5: List of packages
--
-- This schema version adds a table for tracking DAML-LF packages.
-- Previously, packages were only stored in memory and needed to be specified through the CLI.
---------------------------------------------------------------------------------------------------



CREATE TABLE packages (
  -- The unique identifier of the package (the hash of its content)
  package_id         varchar primary key      not null,
  -- Packages are uploaded as DAR files (i.e., in groups)
  -- This field can be used to find out which packages were uploaded together
  upload_id          varchar                  not null,
  -- A human readable description of the package source
  source_description varchar,
  -- The size of the archive payload (i.e., the serialized DAML-LF package), in bytes
  size               bigint                   not null,
  -- The time when the package was added
  known_since        timestamp                not null, -- with time zone
  -- The ledger end at the time when the package was added
  ledger_offset      bigint                   not null,
  -- The DAML-LF archive, serialized using the protobuf message `daml_lf.Archive`.
  --  See also `daml-lf/archive/da/daml_lf.proto`.
  package            bytea                    not null
);

-- PART 6: postgres/V6__External_Ledger_Offset.sql not needed as parameters.external_ledger_offset already in PART 1

---------------------------------------------------------------------------------------------------
-- V3: Contract divulgence
--
-- This schema version splits the contracts table into:
--   contracts_data, only holding contract data
--   contracts, only holding contract metadata
--
-- This is done because for divulged contracts, we only know the contract data,
-- but no other metadata.
---------------------------------------------------------------------------------------------------


-- Move the `contract` column (the serialized contract data) from contracts to contract_data.
CREATE TABLE contract_data (
  id             varchar primary key not null,
  -- the serialized contract value, using the definition in
  -- `daml-lf/transaction/src/main/protobuf/com/digitalasset/daml/lf/value.proto`
  -- and the encoder in `ContractSerializer.scala`.
  contract       bytea               not null
);


---------------------------------------------------------------------------------------------------
-- V5.0: Extract event data
--
-- This schema version adds the tables contract_signatories and contract_observers, and the column contracts_create_event_id
-- to store event related data so that it can be easily retrieved later.

CREATE TABLE contract_signatories (
  contract_id varchar not null,
  signatory   varchar not null,
  foreign key (contract_id) references contracts (id)
);
CREATE UNIQUE INDEX contract_signatories_idx
  ON contract_signatories (contract_id, signatory);


CREATE TABLE contract_observers (
  contract_id varchar not null,
  observer   varchar    not null,
  foreign key (contract_id) references contracts (id)
);
CREATE UNIQUE INDEX contract_observer_idx
  ON contract_observers (contract_id, observer);


---------------------------------------------------------------------------------------------------
-- V7: Add table for ledger configuration changes
--
-- This schema version adds a table for ledger configuration changes and adds the latest
-- configuration to the parameters table.
---------------------------------------------------------------------------------------------------

-- Table for storing a log of ledger configuration changes and rejections.
CREATE TABLE configuration_entries (
  ledger_offset bigint primary key not null,
  recorded_at timestamp not null, -- with time zone

  submission_id varchar not null,
  participant_id varchar not null,
  -- The type of entry, one of 'accept' or 'reject'.
  typ varchar not null,
  -- The configuration that was proposed and either accepted or rejected depending on the type.
  -- Encoded according to participant-state/protobuf/ledger_configuration.proto.
  configuration bytea not null,

  -- If the type is 'rejection', then the rejection reason is set.
  -- Rejection reason is a human-readable description why the change was rejected.
  rejection_reason varchar,

  -- Check that fields are correctly set based on the type.
  constraint configuration_entries_check_reason
  check (
    (typ = 'accept' and rejection_reason is null) or
    (typ = 'reject' and rejection_reason is not null))
);

-- Index for retrieving the configuration entry by submission identifier.
-- To be used for completing configuration submissions.
CREATE UNIQUE INDEX idx_configuration_submission
  ON configuration_entries (submission_id, participant_id);


---------------------------------------------------------------------------------------------------
-- V8: party_entries
--
-- This schema version adds a table for tracking party allocation submissions
---------------------------------------------------------------------------------------------------

CREATE TABLE party_entries
(
  -- The ledger end at the time when the party allocation was added
  ledger_offset    bigint primary key  not null,
  recorded_at      timestamp           not null, --with timezone
  -- SubmissionId for the party allocation
  submission_id    varchar,
  -- participant id that initiated the allocation request
  -- may be null for implicit party that has not yet been allocated
  participant_id   varchar,
  -- party
  party            varchar,
  -- displayName
  display_name     varchar,
  -- The type of entry, 'accept' or 'reject'
  typ              varchar             not null,
  -- If the type is 'reject', then the rejection reason is set.
  -- Rejection reason is a human-readable description why the change was rejected.
  rejection_reason varchar,
  -- true if the party was added on participantId node that owns the party
  is_local         bool,

  constraint check_party_entry_type
    check (
        (typ = 'accept' and rejection_reason is null and party is not null) or
        (typ = 'reject' and rejection_reason is not null)
      )
);

-- Index for retrieving the party allocation entry by submission id per participant
CREATE UNIQUE INDEX idx_party_entries
  ON party_entries (submission_id);


---------------------------------------------------------------------------------------------------
-- V9: package_entries
--
-- This schema version adds a table for tracking DAML-LF package submissions
-- It includes id to track the package submission and status
---------------------------------------------------------------------------------------------------

CREATE TABLE package_entries
(
  ledger_offset    bigint primary key not null,
  recorded_at      timestamp          not null, --with timezone
  -- SubmissionId for package to be uploaded
  submission_id    varchar,
  -- The type of entry, one of 'accept' or 'reject'
  typ              varchar            not null,
  -- If the type is 'reject', then the rejection reason is set.
  -- Rejection reason is a human-readable description why the change was rejected.
  rejection_reason varchar,

  constraint check_package_entry_type
    check (
        (typ = 'accept' and rejection_reason is null) or
        (typ = 'reject' and rejection_reason is not null)
      )
);

-- Index for retrieving the package entry by submission id
CREATE UNIQUE INDEX idx_package_entries
  ON package_entries (submission_id);


---------------------------------------------------------------------------------------------------
-- V11: participant_command_completions

CREATE TABLE participant_command_completions
(
  completion_offset bigint not null,
  record_time timestamp not null,

  application_id varchar, -- null for checkpoints
  submitting_party varchar, -- null for checkpoints
  command_id varchar, -- null for checkpoints

  transaction_id varchar, -- null if the command was rejected and checkpoints
  status_code integer, -- null for successful command and checkpoints
  status_message varchar -- null for successful command and checkpoints
);

CREATE INDEX ON participant_command_completions(completion_offset, application_id, submitting_party);


---------------------------------------------------------------------------------------------------
-- V12: New command deduplication
--
-- Command deduplication has moved from ledger to participant
---------------------------------------------------------------------------------------------------

CREATE TABLE participant_command_submissions(
  -- The deduplication key
  deduplication_key  varchar primary key   not null,
  -- The time the command was first submitted
  submitted_at       timestamp             not null,
  -- The time the command will stop being deduplicated
  ttl                timestamp             not null,
  -- The gRPC status code of the original command submission, if available
  status_code        int,
  -- The gRPC status message of the original command submission, if available
  status_message     varchar
);
