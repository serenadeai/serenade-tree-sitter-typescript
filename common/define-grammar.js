module.exports = function defineGrammar(dialect) {
  return grammar(require('tree-sitter-javascript/grammar'), {
    name: dialect,

    externals: ($, previous) =>
      previous.concat([
        // Allow the external scanner to tell whether it is parsing an expression
        // or a type by checking the validity of this binary operator. This is
        // needed because the rules for automatic semicolon insertion are
        // slightly different when parsing types. Any binary-only operator would
        // work.
        '||',
        $._function_signature_automatic_semicolon,
      ]),

    supertypes: ($, previous) => previous.concat([$._primary_type]),

    precedences: ($, previous) =>
      previous.concat([
        ['call', 'unary', 'binary_as', $.await_expression, $.lambda],
        [
          $.intersection_type,
          $.union_type,
          $.conditional_type,
          $.function_type,
          'binary_as',
          $.type_predicate,
          $.readonly_type,
        ],
        [$.mapped_type_clause, $.primary_expression],
        [$.accessibility_modifier, $.primary_expression],
        ['unary_void', $.expression],
        ['extends_type', $.primary_expression],
        ['unary', 'assign'],
        ['declaration', $.expression],
        [$.predefined_type, $.unary_expression],
        [$.type, $.flow_maybe_type],
        [$.tuple_type, $.array_type, $.pattern, $.type],
        [$.readonly_type, $.pattern],
        [$.readonly_type, $.primary_expression],
        [$.type_query, $.subscript_expression, $.expression],
        [
          $.nested_type_identifier,
          $.generic_type,
          $._primary_type,
          $.lookup_type,
          $.index_type_query,
          $.type,
        ],
      ]),

    conflicts: ($, previous) =>
      previous.concat([
        [$.call, $.binary_expression],
        [$.call, $.binary_expression, $.unary_expression],
        [$.call, $.binary_expression, $.update_expression],
        [$.call, $.binary_expression, $.type_assertion],

        [$.nested_identifier, $.nested_type_identifier, $.primary_expression],
        [$.nested_identifier, $.nested_type_identifier],
        [$.nested_identifier, $.member_expression],

        [$.primary_expression, $.array_type],
        [$.primary_expression, $.array_type, $.tuple_type],

        [$._call_signature, $.function_type],
        [$._call_signature, $.constructor_type],

        [$._primary_type, $.type_parameter],
        [$.markup_opening_tag, $.type_parameter],
        [$.markup_opening_tag, $.type_parameter, $._primary_type],
        [$.markup_opening_tag, $.generic_type],
        [$.jsx_namespace_name, $._primary_type],

        [$.primary_expression, $.parameter_name],
        [$.primary_expression, $.parameter_name, $.predefined_type],
        [$.primary_expression, $.parameter_name, $._primary_type],
        [$.primary_expression, $.parameter_name, $.array_type, $.tuple_type],
        [$.primary_expression, $.literal_type],
        [$.primary_expression, $._primary_type],
        [$.primary_expression, $.generic_type],
        [$.primary_expression, $.predefined_type],
        [$.primary_expression, $.pattern, $._primary_type],
        [$.primary_expression, $.pattern, $.predefined_type],
        [$.parameter_name, $.predefined_type],
        [$.parameter_name, $._primary_type],
        [$.parameter_name, $.assignment],
        [$.parameter_name, $.pattern],
        [$.pattern, $._primary_type],
        [$.pattern, $.predefined_type],

        [$.optional_tuple_parameter, $._primary_type],
        [$.optional_tuple_parameter, $._primary_type, $.primary_expression],
        [$.rest_pattern, $._primary_type, $.primary_expression],
        [$.rest_pattern, $._primary_type],

        [$.object, $.object_type],
        [$.object, $.property_name],
        [$.object, $.object_pattern, $.object_type],
        [$.object, $.object_pattern_element, $.property_name],
        [$.object_pattern, $.object_type],
        [$.object_pattern, $.object_type],
        [$.object_pattern_element, $.property_name],

        [$.array, $.tuple_type],
        [$.array, $.array_pattern, $.tuple_type],
        [$.array_pattern, $.tuple_type],

        // New

        [$.primary_expression, $.readonly_modifier],
        [$.index_signature, $.readonly_modifier],
        [$.readonly_type, $.readonly_modifier],

        [$.jsx_start_opening_element],
        [$.markup_opening_tag],
        [$.markup_opening_tag, $.jsx_identifier_],

        [$.lambda_call_signature, $.function_type],
        [$.lambda_call_signature, $.constructor_type],

        [$.primary_expression, $.type_identifier_],
        [$.pattern, $.type_identifier_],
        [$.optional_tuple_parameter, $.type_identifier_],
        [$.nested_identifier, $.nested_type_identifier, $.type_identifier_],
        [$.primary_expression, $.pattern, $.type_identifier_],
        [$.primary_expression, $.optional_tuple_parameter, $.type_identifier_],
        [$.rest_pattern, $.type_identifier_],
        [$.nested_identifier, $.type_identifier_],
        [$.primary_expression, $.nested_identifier, $.type_identifier_],
        [$.primary_expression, $.rest_pattern, $.type_identifier_],
        [$.jsx_identifier_, $.type_identifier_],
        [$.markup_opening_tag, $.jsx_identifier_, $.type_identifier_],

        [$.reserved_identifier_, $.accessibility_modifier],
        [$.reserved_identifier_, $.readonly_modifier],
        [$.reserved_identifier_, $.readonly_type],
        [$.reserved_identifier_, $.readonly_modifier, $.readonly_type],
        [$.reserved_identifier_, $.predefined_type],

        [$.identifier_or_reserved, $.rest_pattern],
        [$.identifier_or_reserved, $.type_identifier_],
        [$.identifier_or_reserved, $.rest_pattern, $.type_identifier_],
        [
          $.identifier_or_reserved,
          $.optional_tuple_parameter,
          $.type_identifier_,
        ],
        [$.identifier_or_reserved, $.pattern, $.type_identifier_],
        [
          $.identifier_or_reserved,
          $.nested_identifier,
          $.nested_type_identifier,
        ],
        [$.identifier_or_reserved, $.lambda],
      ]),

    inline: ($, previous) =>
      previous
        .filter(rule => !['parameter', '_call_signature'].includes(rule.name))
        .concat([$._enum_member, $.jsx_start_opening_element]),

    rules: {
      // Modifiers

      abstract_modifier: $ => field('modifier', 'abstract'),
      accessibility_modifier: $ =>
        field('modifier', choice('public', 'private', 'protected')),
      const_modifier: $ => field('modifier', 'const'),
      readonly_modifier: $ => field('modifier', 'readonly'),

      public_field_definition: $ =>
        seq(
          optional('declare'),
          optional_with_placeholder(
            'modifier_list',
            seq(
              optional($.accessibility_modifier),
              choice(
                seq(optional($.static_modifier), optional($.readonly_modifier)),
                seq(
                  optional($.abstract_modifier),
                  optional($.readonly_modifier)
                ),
                seq(
                  optional($.readonly_modifier),
                  optional($.abstract_modifier)
                )
              )
            )
          ),
          field(
            'assignment_list',
            alias($.public_field_assignment, $.assignment)
          )
        ),

      public_field_assignment: $ =>
        seq(
          alias($.public_field_assignment_variable, $.assignment_variable),
          optional_with_placeholder(
            'assignment_value_list_optional',
            $.assignment_initializer
          )
        ),

      public_field_assignment_variable: $ =>
        seq(
          $.property_name,
          optional(choice('?', '!')),
          optional_with_placeholder('type_optional', $.type_annotation)
        ),

      // override original catch, add optional type annotation
      catch_parameter: $ =>
        seq(
          choice($.identifier, $._destructuring_pattern),
          optional(
            // only types that resolve to 'any' or 'unknown' are supported
            // by the language but it's simpler to accept any type here.
            field('type', $.type_annotation)
          )
        ),

      call: $ =>
        choice(
          prec(
            'call',
            seq(
              field('identifier', $.expression),
              optional($.type_arguments),
              choice($.arguments, $.template_string)
            )
          ),
          prec(
            'member',
            seq(
              field('function_', $.primary_expression),
              '?.',
              optional($.type_arguments),
              $.arguments
            )
          )
        ),

      new_expression: $ =>
        prec.right(
          'new',
          seq(
            'new',
            field('constructor', $.primary_expression),
            optional($.type_arguments),
            optional($.arguments)
          )
        ),

      augmented_assignment_lhs: ($, previous) =>
        choice(previous, $.non_null_expression),

      _lhs_expression: ($, previous) => choice(previous, $.non_null_expression),

      primary_expression: ($, previous) =>
        choice(previous, $.non_null_expression),

      // If the dialect is regular typescript, we exclude JSX expressions and
      // include type assertions. If the dialect is TSX, we do the opposite.
      expression: ($, previous) => {
        const choices = [$.as_expression, $.internal_module]

        if (dialect === 'typescript') {
          choices.push($.type_assertion)
          choices.push(
            ...previous.members.filter(
              member =>
                member.name !== '_jsx_element' && member.name !== 'jsx_fragment'
            )
          )
        } else if (dialect === 'tsx') {
          choices.push(...previous.members)
        } else {
          throw new Error(`Unknown dialect ${dialect}`)
        }

        return choice(...choices)
      },

      jsx_start_opening_element: $ =>
        seq(
          '<',
          field(
            'identifier',
            choice(
              choice($.jsx_identifier_, $.jsx_namespace_name),
              seq(
                choice($.identifier, $.nested_identifier),
                optional($.type_arguments)
              )
            )
          ),
          optional_with_placeholder(
            'markup_attribute_list',
            repeat($._jsx_attribute)
          )
        ),

      // This rule is only referenced by expression when the dialect is 'tsx'
      markup_opening_tag: $ =>
        prec.dynamic(-1, seq($.jsx_start_opening_element, '>')),

      // tsx only. See markup_opening_tag.
      jsx_self_closing_element: $ =>
        prec.dynamic(-1, seq($.jsx_start_opening_element, '/', '>')),

      _import_export_specifier: ($, previous) =>
        seq(optional(choice('type', 'typeof')), previous),

      import: $ =>
        seq(
          'import',
          optional(choice('type', 'typeof')),
          choice(
            seq($.import_clause, $._from_clause),
            $.import_require_clause,
            $.string
          ),
          $._semicolon
        ),

      export_statement: ($, previous) =>
        choice(
          previous,
          seq('export', 'type', $.export_clause),
          seq('export', '=', $.identifier, $._semicolon),
          seq('export', 'as', 'namespace', $.identifier, $._semicolon)
        ),

      non_null_expression: $ => prec.left('unary', seq($.expression, '!')),

      variable_declarator: $ =>
        choice(
          $.variable_declarator_simple,
          prec(
            'declaration',
            seq(
              field('assignment_variable', $.identifier),
              '!',
              field('type_optional', $.type_annotation),
              optional_with_placeholder(
                'assignment_value_list_optional',
                '!!UNMATCHABLE_52444140579a'
              )
            )
          )
        ),

      variable_declarator_simple: $ =>
        seq(
          field(
            'assignment_variable',
            choice($.identifier, $._destructuring_pattern)
          ),
          optional_with_placeholder('type_optional', $.type_annotation),
          optional_with_placeholder(
            'assignment_value_list_optional',
            $.assignment_initializer
          )
        ),

      method_signature: $ =>
        seq(
          optional_with_placeholder(
            'modifier_list',
            seq(
              optional($.accessibility_modifier),
              optional($.static_modifier),
              optional($.readonly_modifier),
              optional($.async_modifier),
              optional($.accessors_modifier)
            )
          ),
          field('name', $.property_name),
          optional('?'),
          $._call_signature
        ),

      abstract_method_signature: $ =>
        seq(
          field(
            'modifier_list',
            seq(
              optional($.accessibility_modifier),
              $.abstract_modifier,
              optional($.accessors_modifier)
            )
          ),
          field('name', $.property_name),
          optional('?'),
          $._call_signature
        ),

      parenthesized_expression_inner: ($, previous) =>
        choice(
          seq($.expression, optional($.type_annotation)),
          $.sequence_expression
        ),

      parameter: $ => choice($.required_parameter, $.optional_parameter),

      function_signature: $ =>
        seq(
          optional_with_placeholder('modifier_list', $.async_modifier),
          'function',
          field('name', $.identifier),
          $._call_signature,
          choice($._semicolon, $._function_signature_automatic_semicolon)
        ),

      class_body: $ =>
        seq(
          '{',
          optional_with_placeholder(
            'class_member_list',
            repeat(alias($.class_member, $.member))
          ),
          '}'
        ),

      class_member: $ =>
        choice(
          $.decorator,
          // seq(field('method', ), optional($._semicolon)),
          seq(
            choice(
              $.index_signature,
              field(
                'method',
                choice(
                  $.method_definition,
                  $.abstract_method_signature,
                  $.method_signature
                )
              ),
              field('property', $.public_field_definition)
            ),
            choice($._semicolon, ',')
          )
        ),

      method_definition: $ =>
        prec.left(
          seq(
            optional_with_placeholder(
              'modifier_list',
              seq(
                optional($.accessibility_modifier),
                optional($.static_modifier),
                optional($.readonly_modifier),
                optional($.async_modifier),
                optional($.accessors_modifier)
              )
            ),
            field('name', $.property_name),
            optional('?'),
            $._call_signature,
            field('body', $.enclosed_body)
          )
        ),

      declaration: ($, previous) =>
        choice(
          previous,
          $.function_signature,
          $.abstract_class_declaration,
          $.module_declaration,
          prec('declaration', $.internal_module),
          $.type_alias,
          alias($.enum_declaration, $.enum),
          $.interface,
          $.import_alias,
          $.ambient_declaration
        ),

      type_assertion: $ =>
        prec.left('unary', seq($.type_arguments, $.expression)),

      as_expression: $ =>
        prec.left(
          'binary_as',
          seq($.expression, 'as', choice($.type, $.template_string))
        ),

      import_require_clause: $ =>
        seq($.identifier, '=', 'require', '(', $.string, ')'),

      implements_type: $ => $.type,

      implements_clause: $ =>
        seq(
          'implements',
          field('implements_list', commaSep1($.implements_type))
        ),

      ambient_declaration: $ =>
        seq(
          'declare',
          choice(
            $.declaration,
            seq('global', $.enclosed_body),
            seq(
              'module',
              '.',
              alias($.identifier, $.property_identifier),
              ':',
              $.type,
              $._semicolon
            )
          )
        ),

      class: $ =>
        prec(
          'literal',
          seq(
            optional_with_placeholder('decorator_list', repeat($.decorator)),
            'class',
            optional(field('identifier', $.type_identifier_)),
            optional_with_placeholder(
              'type_parameter_list_optional',
              $.type_parameters
            ),
            optional_with_placeholder('extends_optional', $.extends_clause),
            optional_with_placeholder(
              'implements_list_optional',
              $.implements_clause
            ),
            field('enclosed_body', $.class_body)
          )
        ),

      abstract_class_declaration: $ =>
        prec(
          'declaration',
          seq(
            optional_with_placeholder('decorator_list', repeat($.decorator)),
            field('modifier_list', $.abstract_modifier),
            'class',
            field('identifier', $.type_identifier_),
            optional_with_placeholder(
              'type_parameter_list_optional',
              $.type_parameters
            ),
            optional_with_placeholder('extends_optional', $.extends_clause),
            optional_with_placeholder(
              'implements_list_optional',
              $.implements_clause
            ),
            field('enclosed_body', $.class_body)
          )
        ),

      class_declaration: $ =>
        prec.left(
          'declaration',
          seq(
            optional_with_placeholder('decorator_list', repeat($.decorator)),
            'class',
            field('identifier', $.type_identifier_),
            optional_with_placeholder(
              'type_parameter_list_optional',
              $.type_parameters
            ),
            optional_with_placeholder('extends_optional', $.extends_clause),
            optional_with_placeholder(
              'implements_list_optional',
              $.implements_clause
            ),
            field('enclosed_body', $.class_body),
            optional($._automatic_semicolon)
          )
        ),

      module_declaration: $ => seq('module', $.module),

      internal_module: $ => seq('namespace', $.module),

      module: $ =>
        prec.right(
          seq(
            field(
              'identifier',
              choice($.string, $.identifier, $.nested_identifier)
            ),
            optional($.enclosed_body)
          )
        ),

      import_alias: $ =>
        seq(
          'import',
          $.identifier,
          '=',
          choice($.identifier, $.nested_identifier),
          $._semicolon
        ),

      nested_type_identifier: $ =>
        prec(
          'member',
          seq(
            field('module', choice($.identifier, $.nested_identifier)),
            '.',
            field('name', $.type_identifier_)
          )
        ),

      interface: $ =>
        seq(
          'interface',
          field('identifier', $.type_identifier_),
          optional_with_placeholder(
            'type_parameter_list_optional',
            $.type_parameters
          ),
          optional_with_placeholder('extends_optional', $.extends_clause),
          alias($.interface_brace_enclosed_body, $.enclosed_body)
        ),

      interface_brace_enclosed_body: $ =>
        seq(
          choice('{', '{|'),
          optional_with_placeholder(
            'interface_member_list',
            $.interface_member_list
          ),
          choice('}', '|}')
        ),

      interface_member_list: $ =>
        seq(
          optional(choice(',', ';')),
          sepBy1(
            choice(',', $._semicolon),
            alias($.interface_member, $.member)
          ),
          optional(choice(',', $._semicolon))
        ),

      interface_member: $ =>
        choice(
          $.export_statement,
          field('property', $.property_signature),
          field('method', choice($.method_signature, $.call_signature)),
          $.construct_signature,
          $.index_signature
        ),

      extends_clause: $ =>
        prec(
          'extends_type',
          seq('extends', field('extends_type', commaSep1($.extends_type)))
        ),

      extends_type: $ =>
        choice(
          prec(
            'extends_type',
            choice($.type_identifier_, $.nested_type_identifier, $.generic_type)
          ),
          $.expression
        ),

      enum_declaration: $ =>
        seq(
          optional_with_placeholder('modifier_list', $.const_modifier),
          'enum',
          field('name', $.identifier),
          field('enclosed_body', $.enum_body)
        ),

      enum_body: $ =>
        seq(
          '{',
          optional_with_placeholder(
            'enum_member_list',
            $.enum_member_list_inner
          ),
          '}'
        ),

      enum_member_list_inner: $ =>
        seq(
          sepBy1(
            ',',
            field('member', choice($.property_name, $.enum_constant))
          ),
          optional(',')
        ),

      enum_constant: $ => seq($.property_name, $.assignment_initializer),

      type_alias: $ =>
        seq(
          'type',
          field('name', $.type_identifier_),
          optional_with_placeholder(
            'type_parameter_list_optional',
            $.type_parameters
          ),
          '=',
          field('value', $.type),
          $._semicolon
        ),

      required_parameter: $ =>
        seq(
          $.parameter_name,
          optional_with_placeholder('type_optional', $.type_annotation),
          optional(seq('=', field('parameter_value', $.expression)))
        ),

      optional_parameter: $ =>
        seq(
          $.parameter_name,
          '?',
          optional_with_placeholder('type_optional', $.type_annotation),
          optional(seq('=', field('parameter_value', $.expression)))
        ),

      parameter_name: $ =>
        seq(
          optional_with_placeholder('decorator_list', repeat($.decorator)),
          optional_with_placeholder(
            'modifier_list',
            seq(
              optional($.accessibility_modifier),
              optional($.readonly_modifier)
            )
          ),
          field('identifier', choice($.pattern, $.this))
        ),

      omitting_type_annotation: $ => seq('-?:', $.type),
      opting_type_annotation: $ => seq('?:', $.type),
      type_annotation: $ => seq(':', $.type),

      asserts: $ =>
        seq(
          ':',
          field(
            'type',
            seq('asserts', choice($.type_predicate, $.identifier, $.this))
          )
        ),

      type: $ =>
        choice(
          $._primary_type,
          $.union_type,
          $.intersection_type,
          $.function_type,
          $.readonly_type,
          $.constructor_type,
          $.infer_type
        ),

      tuple_parameter: $ =>
        seq(choice($.identifier, $.rest_pattern), $.type_annotation),

      optional_tuple_parameter: $ => seq($.identifier, '?', $.type_annotation),

      optional_type: $ => seq($.type, '?'),
      rest_type: $ => seq('...', $.type),

      _tuple_type_member: $ =>
        choice(
          alias($.tuple_parameter, $.required_parameter),
          alias($.optional_tuple_parameter, $.optional_parameter),
          $.optional_type,
          $.rest_type,
          $.type
        ),

      constructor_type: $ =>
        prec.left(
          seq(
            'new',
            optional_with_placeholder(
              'type_parameter_list_optional',
              $.type_parameters
            ),
            $.formal_parameters,
            '=>',
            $.type
          )
        ),

      _primary_type: $ =>
        choice(
          $.parenthesized_type,
          $.predefined_type,
          $.type_identifier_,
          $.nested_type_identifier,
          $.generic_type,
          $.object_type,
          $.array_type,
          $.tuple_type,
          $.flow_maybe_type,
          $.type_query,
          $.index_type_query,
          $.this,
          $.existential_type,
          $.literal_type,
          $.lookup_type,
          $.conditional_type
        ),

      infer_type: $ => seq('infer', $.type_identifier_),

      conditional_type: $ =>
        prec.left(
          seq(
            field('left', $.type),
            'extends',
            field('right', $.type),
            '?',
            field('consequence', $.type),
            ':',
            field('alternative', $.type)
          )
        ),

      generic_type: $ =>
        prec(
          'call',
          seq(
            choice($.type_identifier_, $.nested_type_identifier),
            $.type_arguments
          )
        ),

      type_predicate: $ =>
        field('type', seq(choice($.identifier, $.this), 'is', $.type)),

      type_predicate_annotation: $ => seq(seq(':', $.type_predicate)),

      type_query: $ =>
        prec.right(seq('typeof', choice($.primary_expression, $.generic_type))),

      index_type_query: $ => seq('keyof', $._primary_type),

      lookup_type: $ => seq($._primary_type, '[', $.type, ']'),

      mapped_type_clause: $ => seq($.type_identifier_, 'in', $.type),

      literal_type: $ =>
        choice(
          alias($._number, $.unary_expression),
          $.number,
          $.string,
          $.true,
          $.false
        ),

      _number: $ =>
        prec.left(
          1,
          seq(field('operator', choice('-', '+')), field('argument_', $.number))
        ),

      existential_type: $ => '*',

      flow_maybe_type: $ => prec.right(seq('?', $._primary_type)),

      parenthesized_type: $ => seq('(', $.type, ')'),

      predefined_type: $ =>
        choice('any', 'number', 'boolean', 'string', 'symbol', 'void'),

      type_arguments: $ =>
        seq(
          '<',
          field(
            'type_argument_list',
            commaSep1(alias($.type, 'type_argument'))
          ),
          optional(','),
          '>'
        ),

      object_type: $ =>
        seq(
          choice('{', '{|'),
          optional(
            seq(
              optional(choice(',', ';')),
              sepBy1(choice(',', $._semicolon), $.object_type_content),
              optional(choice(',', $._semicolon))
            )
          ),
          choice('}', '|}')
        ),

      object_type_content: $ =>
        choice(
          $.export_statement,
          $.property_signature,
          $.call_signature,
          $.construct_signature,
          $.index_signature,
          $.method_signature
        ),

      call_signature: $ => $._call_signature,

      property_signature: $ =>
        seq(
          optional_with_placeholder(
            'modifier_list',
            seq(
              optional($.accessibility_modifier),
              optional($.static_modifier),
              optional($.readonly_modifier)
            )
          ),
          field(
            'assignment_list',
            alias($.property_signature_assignment, $.assignment)
          )
        ),

      property_signature_assignment: $ =>
        seq(
          alias(
            $.property_signature_assignment_variable,
            $.assignment_variable
          ),
          optional_with_placeholder(
            'assignment_value_list_optional',
            '!!UNMATCHABLE_34ed59dbebf4'
          )
        ),

      property_signature_assignment_variable: $ =>
        seq(
          $.property_name,
          optional('?'),
          optional_with_placeholder(
            'type_optional',
            optional($.type_annotation)
          )
        ),

      _call_signature: $ =>
        seq(
          optional_with_placeholder(
            'type_parameter_list_optional',
            $.type_parameters
          ),
          field('parameters', $.formal_parameters),
          optional_with_placeholder(
            'return_type_optional',
            choice($.type_annotation, $.asserts, $.type_predicate_annotation)
          )
        ),

      lambda_call_signature: $ =>
        seq(
          optional_with_placeholder(
            'type_parameter_list_optional',
            $.type_parameters
          ),
          field('parameter_list_optional', $.formal_parameters),
          optional_with_placeholder(
            'return_type_optional',
            choice($.type_annotation, $.asserts, $.type_predicate_annotation)
          )
        ),

      type_parameters: $ =>
        seq(
          '<',
          field('type_parameter_list', commaSep1($.type_parameter)),
          optional(','),
          '>'
        ),

      type_parameter: $ =>
        seq(
          $.type_identifier_,
          optional($.constraint),
          optional($.default_type)
        ),

      default_type: $ => seq('=', $.type),

      constraint: $ => seq(choice('extends', ':'), $.type),

      construct_signature: $ =>
        seq(
          'new',
          optional_with_placeholder(
            'type_parameter_list_optional',
            $.type_parameters
          ),
          $.formal_parameters,
          optional($.type_annotation)
        ),

      index_signature: $ =>
        seq(
          optional(seq(optional(field('sign', '-')), 'readonly')),
          '[',
          choice(
            seq($.identifier_or_reserved, ':', $.type),
            $.mapped_type_clause
          ),
          ']',
          choice(
            $.type_annotation,
            $.omitting_type_annotation,
            $.opting_type_annotation
          )
        ),

      array_type: $ => seq($._primary_type, '[', ']'),
      tuple_type: $ =>
        seq('[', commaSep($._tuple_type_member), optional(','), ']'),
      readonly_type: $ => seq('readonly', $.type),

      union_type: $ => prec.left(seq(optional($.type), '|', $.type)),
      intersection_type: $ => prec.left(seq(optional($.type), '&', $.type)),

      function_type: $ =>
        prec.left(
          seq(
            optional_with_placeholder(
              'type_parameter_list_optional',
              $.type_parameters
            ),
            $.formal_parameters,
            '=>',
            choice($.type, $.type_predicate)
          )
        ),

      type_identifier_: $ => alias($.identifier, $.type_identifier),

      reserved_identifier_: ($, previous) =>
        choice(
          'declare',
          'namespace',
          'type',
          'public',
          'private',
          'protected',
          'readonly',
          'module',
          'any',
          'number',
          'boolean',
          'string',
          'symbol',
          'export',
          previous
        ),
    },
  })
}

function commaSep1(rule) {
  return sepBy1(',', rule)
}

function commaSep(rule) {
  return sepBy(',', rule)
}

function sepBy(sep, rule) {
  return optional(sepBy1(sep, rule))
}

function sepBy1(sep, rule) {
  return seq(rule, repeat(seq(sep, rule)))
}

function optional_with_placeholder(field_name, rule) {
  return choice(field(field_name, rule), field(field_name, blank()))
}
