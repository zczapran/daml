module Foo.Foo (foo) where

#include <somelib.h>

foo :: Int
foo = {# sizeof some_type_t #}
