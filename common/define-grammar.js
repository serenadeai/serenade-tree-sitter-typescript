module.exports = function defineGrammar(dialect) {
  return grammar(require('tree-sitter-javascript/grammar'), {
    name: dialect,

    externals: ($, previous) => previous.concat([
      // Allow the external scanner to tell whether it is parsing an expression
      // or a type by checking the validity of this binary operator. This is
      // needed because the rules for automatic semicolon insertion are
      // slightly different when parsing types. Any binary-only operator would
      // work.
      '||',
      $._function_signature_automatic_semicolon,
    ]),

    supertypes: ($, previous) => previous.concat([
      $._primary_type,
    ]),

    precedences: ($, previous) => previous.concat([
      [
        'call',
        'unary',
        'binary_as',
        $.await_expression,
        $.lambda,
      ],
      [
        $.intersection_type,
        $.union_type,
        $.conditional_type,
        $.function_type,
        'binary_as',
        $.type_predicate,
        $.readonly_type
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
      [$.nested_type_identifier, $.generic_type, $._primary_type, $.lookup_type, $.index_type_query, $.type],
      // [$.type_alias, $.identifier_or_reserved_identifier],
      // [$.abstract_class_declaration, $.abstract_method_signature, $.abstract_modifier],
      // [$.export_statement, $.class, $.class_declaration],
      ['keyword', 'modifier', $.type_parameter], 
      [$.assignment_variable, $.type_parameter], 
      [$.identifier_or_reserved_identifier, $.type_alias],

      //   $.abstract_modifier,
      //   $.accessibility_modifier, 
      //   $.async_modifier, 
      //   $.const_modifier, 
      //   $.get_modifier, 
      //   $.readonly_modifier,
      //   $.set_modifier, 
      //   $.static_modifier,
      //   $.symbol_star_modifier,

      //   // More specific than latter
      //   $.assignment_variable, 
      //   $., 
      //   $., 

      //   $.
      // ],
    ]),

    conflicts: ($, previous) => previous.concat([
      [$.call, $.binary_expression],
      [$.call, $.binary_expression, $.unary_expression],
      [$.call, $.binary_expression, $.update_expression],
      [$.call, $.binary_expression, $.type_assertion],

      [$.nested_identifier, $.nested_type_identifier, $.primary_expression],
      [$.nested_identifier, $.nested_type_identifier],
      [$.nested_identifier, $.member_expression],

      [$.primary_expression, $.array_type],
      [$.primary_expression, $.array_type, $.tuple_type],

      // [$.call_signature_, $.function_type],
      // [$.call_signature_, $.constructor_type],

      [$._primary_type, $.type_parameter],
      [$.jsx_opening_element, $.type_parameter],
      [$.jsx_opening_element, $.type_parameter, $._primary_type],
      [$.jsx_opening_element, $.generic_type],
      [$.jsx_namespace_name, $._primary_type],

      [$.primary_expression, $.parameter_name],
      [$.primary_expression, $.parameter_name, $.predefined_type],
      [$.primary_expression, $.parameter_name, $._primary_type],
      [$.primary_expression, $.parameter_name, $.array_type, $.tuple_type],
      [$.primary_expression, $.readonly_modifier],
      [$.primary_expression, $.async_modifier],
      [$.primary_expression, $.static_modifier],
      [$.primary_expression, $.get_modifier],
      [$.primary_expression, $.set_modifier],
      [$.primary_expression, $.literal_type],
      [$.primary_expression, $._primary_type],
      [$.primary_expression, $.generic_type],
      [$.primary_expression, $.predefined_type],
      [$.primary_expression, $.pattern, $._primary_type],
      [$.primary_expression, $.pattern, $.predefined_type],
      [$.primary_expression, $.pattern, $.readonly_modifier],
      [$.primary_expression, $.pattern, $.async_modifier],
      [$.primary_expression, $.labeled_statement, $.property_name],
      [$.primary_expression, $.labeled_statement, $._primary_type],
      [$.primary_expression, $.labeled_statement, $.predefined_type],
      [$.property_name, $.module_declaration],
      [$.property_name, $.internal_module],
      [$.identifier_or_reserved_identifier, $.public_field_definition],

      [$.parameter_name, $.predefined_type],
      [$.parameter_name, $._primary_type],
      [$.parameter_name, $.assignment],
      [$.parameter_name, $.pattern],
      [$.pattern, $._primary_type],
      [$.pattern, $.predefined_type],
      [$.pattern, $.readonly_modifier],

      [$.optional_tuple_parameter, $._primary_type],
      [$.optional_tuple_parameter, $._primary_type, $.primary_expression],
      [$.rest_pattern, $._primary_type, $.primary_expression],
      [$.rest_pattern, $._primary_type],

      [$.object, $.object_type],
      [$.object, $.property_name],
      [$.object, $.object_pattern, $.object_type],
      [$.object, $.object_pattern, $.property_name],
      [$.object_pattern, $.object_type],
      [$.object_pattern, $.object_type],
      [$.object_pattern, $.property_name],
      [$.object, $.object_type, $.brace_enclosed_body],
      [$.object_type, $.empty_statement],
      [$.object_type, $.brace_enclosed_body],
      [$.interface_member, $.brace_enclosed_body],

      [$.list, $.tuple_type],
      [$.list, $.array_pattern, $.tuple_type],
      [$.array_pattern, $.tuple_type],
      
      [$.named_function, $.type_parameter],
      [$.function_declaration, $.function_signature, $.type_parameter],
      [$.function_declaration, $.function_signature, $.type_parameter, $.named_function],
      [$.generator_function, $.type_parameter],
      [$.generator_function, $.generator_function_declaration, $.type_parameter],
      [$.function_declaration, $.generator_function_declaration, $.async_modifier],
      [$.function, $.type_parameter],
      [$.generator_function_declaration, $.async_modifier],
      [$.lambda, $.async_modifier],
      [$.method_signature, $.property_name],
      [$.export_statement, $.assignment_variable],
      [$.export_statement, $.object_assignment_pattern, $.assignment_variable],
      [$.primary_expression, $.assignment_variable],
      [$.primary_expression, $.assignment_variable, $.type_parameter],
      [$.primary_expression, $.type_parameter],
      [$.primary_expression, $.type_parameter, $.labeled_statement],
      [$.primary_expression, $.type_parameter, $.property_name],
      [$.primary_expression, $.type_parameter, $.property_name, $.labeled_statement],
      // [$.primary_expression, $.method_definition, $.method_signature],
      [$.primary_expression, $.identifier_or_reserved_identifier, $.assignment_variable],
      [$.primary_expression, $.identifier_or_reserved_identifier, $.type_parameter, $.assignment_variable],
      [$.primary_expression, $.identifier_or_reserved_identifier, $.type_parameter],
      [$.primary_expression, $.identifier_or_reserved_identifier, $.generic_type],
      [$.primary_expression, $.identifier_or_reserved_identifier, $._primary_type],
      [$.primary_expression, $.identifier_or_reserved_identifier, $.optional_tuple_parameter, $._primary_type],
      [$.primary_expression, $.identifier_or_reserved_identifier, $.rest_pattern, $._primary_type],
      [$.primary_expression, $.identifier_or_reserved_identifier, $.type_parameter, $._primary_type],

      // [$.identifier_or_reserved_identifier, $.method_definition, $.lambda],
      // [$.identifier_or_reserved_identifier, $.method_definition, $.method_signature],
      [$.identifier_or_reserved_identifier, $.internal_module],
      [$.identifier_or_reserved_identifier, $.module_declaration],
      [$.identifier_or_reserved_identifier, $.type_parameter],
      [$.identifier_or_reserved_identifier, $.method_signature],
      [$.identifier_or_reserved_identifier, $._primary_type],
      [$.identifier_or_reserved_identifier, $.predefined_type],
      [$.identifier_or_reserved_identifier, $.tuple_parameter],
      [$.identifier_or_reserved_identifier, $.tuple_parameter, $.type_parameter],
      [$.identifier_or_reserved_identifier, $.nested_identifier, $.nested_type_identifier, $.primary_expression],
      // [$.identifier_or_reserved_identifier, $.method_signature, $.lambda, $.method_definition],

      [$.assignment_variable, $.identifier_or_reserved_identifier, $.export_statement],

      [$.assignment_variable, $.predefined_type],
      [$.assignment_variable, $.type_parameter, $.rest_pattern],
      [$.assignment_variable, $.type_parameter, $._primary_type],
      [$.type_parameter, $.rest_pattern],
      [$.assignment_variable, $.type_parameter, $.pattern],
      [$.primary_expression, $.assignment_variable, $.pattern],
      [$.primary_expression, $.type_parameter, $.pattern],
      [$.primary_expression, $.type_parameter, $._primary_type],
      [$.primary_expression, $.type_parameter, $._primary_type, $.labeled_statement],
      [$.type_parameter, $.pattern],
      [$.type_parameter, $.assignment_variable, $.object_assignment_pattern],
      [$.export_statement, $.class, $.class_declaration, $.method_definition, $.abstract_class_declaration],
      [$.export_statement, $.class, $.class_declaration, $.abstract_class_declaration],
      [$.export_statement, $.method_definition],
      [$.pattern, $.tuple_parameter],
      [$.pattern, $.tuple_parameter, $.type_parameter],
      [$.pattern, $.index_signature, $.type_parameter],
      [$.pattern, $.index_signature],

      // [$.function, $.call_signature_],

      [$.jsx_start_opening_element, $.type_parameter],
      [$.jsx_start_opening_element],
      [$.jsx_opening_element],
      [$.jsx_self_closing_element],
      [$._jsx_identifier, $.jsx_start_opening_element],
      [$._jsx_identifier, $.type_parameter],
      [$._jsx_identifier, $.jsx_start_opening_element, $.type_parameter],

      // [$.assignment_initializer, $.binary_expression]
      [$.statement_list, $.interface_member],
      [$.statement, $.interface_member],
    ]),

    inline: ($, previous) => previous
      .concat([
        $._type_identifier,
        $._enum_member,
        // $.jsx_start_opening_element,  // Turning this off makes it only work for tsx, which we use.
      ]),

    rules: {
      abstract_modifier: $ => prec('modifier', field('modifier', 'abstract')),
      accessibility_modifier: $ => prec('modifier', field('modifier', choice(
        'public',
        'private',
        'protected'
      ))),
      const_modifier: $ => prec('modifier', field('modifier', 'const')),
      readonly_modifier: $ => prec('modifier', field('modifier', 'readonly')),

      public_field_definition: $ => prec.left(seq(
        choice(
          optional_with_placeholder('decorator_list', repeat($.decorator)), 
          optional('declare'),
        ),
        optional_with_placeholder('modifier_list', seq(
          optional($.accessibility_modifier),
          choice(
            seq(optional($.static_modifier), optional($.readonly_modifier)),
            seq(optional($.abstract_modifier), optional($.readonly_modifier)),
            seq(optional($.readonly_modifier), optional($.abstract_modifier)),
          )
        )),
        prec.dynamic(1, field('assignment_variable', $.property_name)),
        optional(choice('?', '!')),
        optional_with_placeholder('type_optional', $.type_annotation),
        optional($.assignment_initializer), 
        optional(';')
        
      )),

      catch_parameter: $ => seq(
        field('parameter_',
          choice($.identifier, $._destructuring_pattern)
        ),
        optional(
          // only types that resolve to 'any' or 'unknown' are supported
          // by the language but it's simpler to accept any type here.
          field('type', $.type_annotation),
        )
      ),

      // override original catch, add optional type annotation
      catch: $ => seq(
        'catch',
        optional(
          seq(
            '(',
            $.catch_parameter,
            ')'
          )
        ),
        $.brace_enclosed_body
      ),

      call: $ => choice(
        prec('call', seq(
          field('identifier', $.expression),
          field('type_arguments', optional($.type_arguments)),
          field('argument_list_parens', choice($.arguments, $.template_string))
        )),
        prec('member', seq(
          field('identifier', $.primary_expression),
          '?.',
          field('type_arguments', optional($.type_arguments)),
          field('argument_list_parens', $.arguments)
        ))
      ),

      new_expression: $ => prec.right('new', seq(
        'new',
        field('constructor', $.primary_expression),
        field('type_arguments', optional($.type_arguments)),
        field('argument_list_parens', optional($.arguments))
      )),

      _augmented_assignment_lhs: ($, previous) => choice(previous, $.non_null_expression),

      _lhs_expression: ($, previous) => choice(previous, $.non_null_expression),

      primary_expression: ($, previous) => choice(
        previous,
        $.non_null_expression,
      ),

      // If the dialect is regular typescript, we exclude JSX expressions and
      // include type assertions. If the dialect is TSX, we do the opposite.
      expression: ($, previous) => {
        const choices = [
          $.as_expression,
          $.internal_module,
        ];

        if (dialect === 'typescript') {
          choices.push($.type_assertion);
          choices.push(...previous.members.filter(member =>
            member.name !== '_jsx_element' && member.name !== 'jsx_fragment'
          ));
        } else if (dialect === 'tsx') {
          choices.push(...previous.members);
        } else {
          throw new Error(`Unknown dialect ${dialect}`);
        }

        return choice(...choices);
      },

      // We override this since the modifiers are defined here. 
      function_declaration: $ => prec.right('declaration', seq(
        optional_with_placeholder('modifier_list', $.async_modifier),
        'function',
        field('function_name', $.identifier),
        $.call_signature_,
        $.brace_enclosed_body,
        optional($.automatic_semicolon)
      )),

      jsx_start_opening_element: $ => seq(
        '<',
        choice(
          field('name_single', choice( // Test to disambiguate 'name' node. 
            $._jsx_identifier,
            $.jsx_namespace_name
          )),
          seq(
            field('name', choice(
              $.identifier,
              $.nested_identifier
            )),
            field('type_arguments', optional($.type_arguments))
          )
        ),
        optional_with_placeholder('jsx_attribute_list', repeat($._jsx_attribute))
      ),

      // This rule is only referenced by expression when the dialect is 'tsx'
      jsx_opening_element: $ => prec.dynamic(-1, seq(
        $.jsx_start_opening_element,
        '>'
      )),

      // tsx only. See jsx_opening_element.
      jsx_self_closing_element: $ => prec.dynamic(-1, seq(
        $.jsx_start_opening_element,
        '/',
        '>'
      )),

      _import_export_specifier: ($, previous) => seq(
        optional(choice('type', 'typeof')),
        previous
      ),

      import: $ => seq(
        'import',
        optional(choice('type', 'typeof')),
        choice(
          seq($.import_clause, $._from_clause),
          $.import_require_clause,
          $.string
        ),
        $._semicolon
      ),

      export_statement: ($, previous) => choice(
        previous,
        seq('export', 'type', $.export_clause),
        seq('export', '=', $.identifier),
        seq('export', 'as', 'namespace', $.identifier)
      ),

      non_null_expression: $ => prec.left('unary', seq(
        $.expression, '!'
      )),

      assignment_pattern: $ => seq(
        field('identifier', $.pattern),
        optional_with_placeholder('type_optional', $.type_annotation),
        '=',
        field('right', $.expression)
      ),

      assignment_variable_declarator: $ => choice($.identifier, $._destructuring_pattern), 

      variable_declarator: $ => choice(
        seq(
          field('assignment_variable', $.assignment_variable_declarator),
          optional_with_placeholder('', $.type_annotation),
          optional($.assignment_initializer)
        ),
        prec('declaration', seq(
          field('assignment_variable', $.identifier),
          '!',
          field('type', $.type_annotation)
        ))
      ),

      method_signature: $ => seq(
        optional_with_placeholder('modifier_list', seq(
          optional($.accessibility_modifier),
          optional($.static_modifier),
          optional($.readonly_modifier),
          optional($.async_modifier),
          optional(choice($.get_modifier, $.set_modifier, $.symbol_star_modifier))
        )),
        prec.dynamic(10, field('name', $.property_name)),
        optional('?'),
        $.call_signature_
      ),

      abstract_method_signature: $ => seq(
        optional($.accessibility_modifier),
        $.abstract_modifier,
        optional(choice($.get_modifier, $.set_modifier, $.symbol_star_modifier)),
        field('name', $.property_name),
        optional('?'),
        $.call_signature_
      ),

      expressions: ($, previous) => choice(
        seq($.expression, optional($.type_annotation)),
        $.sequence_expression
      ),

      parameter: $ => choice(
        $.required_parameter,
        $.optional_parameter
      ),

      function_signature: $ => seq(
        optional_with_placeholder('modifier_list', $.async_modifier),
        'function',
        field('name', $.identifier),
        $.call_signature_,
        choice($._semicolon, $._function_signature_automatic_semicolon),
      ),

      class_member: $ => choice(
        prec.dynamic(-10, seq($.method_definition, optional($._semicolon))),
        prec.dynamic(10, seq(
          choice(
            field('property', prec.dynamic(10, $.public_field_definition)),
            $.abstract_method_signature,
            $.index_signature,
            field('method', $.method_signature)
          ),
          choice($._semicolon, ',')
        )),
        prec(-1, $.decorator)
      ), 
      
      class_body: $ => seq(
        '{',
        optional_with_placeholder(
          'class_member_list', 
          repeat($.class_member)
        ),
        '}'
      ),

      // decorator
      method_definition: $ => seq(
        optional_with_placeholder('decorator_list', repeat($.decorator)),
        optional_with_placeholder('modifier_list', seq(
          optional($.accessibility_modifier),
          optional($.static_modifier),
          optional($.readonly_modifier),
          optional($.async_modifier),
          optional(choice($.get_modifier, $.set_modifier, $.symbol_star_modifier))
        )),
        field('identifier', $.property_name),
        optional('?'),
        $.call_signature_,
        $.brace_enclosed_body
      ),

      declaration: ($, previous) => choice(
        previous,
        $.function_signature,
        $.abstract_class_declaration,
        $.module_declaration,
        prec('declaration', $.internal_module),
        $.type_alias,
        $.enum,
        $.interface,
        $.import_alias,
        $.ambient_declaration
      ),

      type_assertion: $ => prec.left('unary', seq(
        $.type_arguments,
        $.expression
      )),

      as_expression: $ => prec.left('binary_as', seq(
        $.expression,
        'as',
        choice($.type, $.template_string)
      )),

      // class_heritage: not used from javascript grammar. 

      import_require_clause: $ => seq($.identifier, '=', 'require', '(', $.string, ')'),

      implements_type: $ => $.type, 
      
      implements_list_optional: $ => seq(
        'implements',
        field('implements_list', commaSep1($.implements_type))
      ),

      ambient_declaration: $ => seq(
        'declare',
        choice(
          $.declaration,
          seq('global', $.brace_enclosed_body),
          seq('module', '.', $.identifier, ':', $.type, $._semicolon)
        )
      ),

      class: $ => prec('literal', seq(
        optional_with_placeholder('decorator_list', repeat($.decorator)),
        'class',
        field('name', optional($._type_identifier)),
        optional_with_placeholder('type_parameters', $.type_parameters),
        optional_with_placeholder('extends_optional', $.extends_clause),
        optional_with_placeholder('implements_list_optional', 
          alias($.implements_list_optional, $.implements_list_present)), 
        field('brace_enclosed_body', $.class_body)
      )),

      abstract_class_declaration: $ => prec('declaration', seq(
        optional_with_placeholder('decorator_list', repeat($.decorator)),
        $.abstract_modifier,
        'class',
        field('name', $._type_identifier),
        optional_with_placeholder('type_parameters', $.type_parameters),
        optional_with_placeholder('extends_optional', $.extends_clause),
        optional_with_placeholder('implements_list_optional', 
          alias($.implements_list_optional, $.implements_list_present)), 
        field('brace_enclosed_body', $.class_body)
      )),

      class_declaration: $ => prec.left('declaration', seq(
        optional_with_placeholder('decorator_list', repeat($.decorator)),
        'class',
        field('identifier', $._type_identifier),
        optional_with_placeholder('type_parameters', $.type_parameters),
        optional_with_placeholder('extends_optional', $.extends_clause),
        optional_with_placeholder('implements_list_optional', 
          alias($.implements_list_optional, $.implements_list_present)), 
        field('brace_enclosed_body', $.class_body),
        optional($.automatic_semicolon)
      )),

      module_declaration: $ => seq(
        'module',
        $.module
      ),

      internal_module: $ => seq(
        'namespace',
        $.module
      ),

      module: $ => prec.right(seq(
        field('identifier', choice($.string, $.identifier, $.nested_identifier)),
        field('body', optional($.brace_enclosed_body))
      )),

      import_alias: $ => seq(
        'import',
        $.identifier,
        '=',
        choice($.identifier, $.nested_identifier),
        $._semicolon
      ),

      nested_type_identifier: $ => prec('member', seq(
        field('module', choice($.identifier, $.nested_identifier)),
        '.',
        field('name', $._type_identifier)
      )),

      interface: $ => seq(
        'interface',
        field('identifier', $._type_identifier),
        optional_with_placeholder('type_parameters', $.type_parameters),
        optional_with_placeholder('extends_optional', $.extends_clause),
        field('brace_enclosed_body', $.object_type)
      ),

      extends_clause: $ => prec('extends_type', seq(
        'extends',
        field('extends_type', commaSep1(choice(
          prec('extends_type', choice(
            $._type_identifier,
            $.nested_type_identifier,
            $.generic_type
          )),
          $.expression
        )))
      )),

      enum: $ => seq(
        optional_with_placeholder('modifier_list', $.const_modifier),
        'enum',
        $.identifier,
        field('brace_enclosed_body', $.enum_body)
      ),
      
      enum_body: $ => seq(
        '{',
        optional_with_placeholder('enum_member_list', $.enum_member_list_inner),
        '}'
      ), 

      enum_member_list_inner: $ => seq(
        sepBy1(',', 
          choice(
            $.property_name,
            $.enum_constant
          )
        ),
        optional(',')
      ),

      enum_constant: $ => seq(
        $.property_name,
        $.assignment_initializer
      ),

      type_alias: $ => seq(
        'type',
        field('name', $._type_identifier),
        optional_with_placeholder('type_parameters', $.type_parameters),
        '=',
        field('value', $.type)
      ),

      required_parameter: $ => seq(
        $.parameter_name,
        optional_with_placeholder('type_optional', $.type_annotation),
        optional(seq(
          '=', 
          field('parameter_value', $.expression)
        ))
      ),

      optional_parameter: $ => seq(
        $.parameter_name,
        '?',
        optional_with_placeholder('type_optional', $.type_annotation),
        optional(seq(
          '=', 
          field('parameter_value', $.expression)
        ))
      ),

      parameter_name: $ => seq(
        optional_with_placeholder('decorator_list', repeat($.decorator)),
        optional_with_placeholder('modifier_list', seq(
          optional($.accessibility_modifier),
          optional($.readonly_modifier),
        )),
        field('identifier', choice($.pattern, $.this))
      ),

      omitting_type_annotation: $ => seq('-?:', $.type),
      opting_type_annotation: $ => seq('?:', $.type),
      type_annotation: $ => seq(':', $.type),

      asserts: $ => seq(
        ':',
        'asserts',
        choice($.type_predicate, $.identifier, $.this)
      ),

      type: $ => choice(
        $._primary_type,
        $.union_type,
        $.intersection_type,
        $.function_type,
        $.readonly_type,
        $.constructor_type,
        $.infer_type
      ),

      tuple_parameter: $ => seq(
        choice($.identifier, $.rest_pattern),
        $.type_annotation
      ),

      optional_tuple_parameter: $ => seq(
        $.identifier,
        '?',
        $.type_annotation
      ),

      optional_type: $ => seq($.type, '?'),
      rest_type: $ => seq('...', $.type),

      _tuple_type_member: $ => choice(
        alias($.tuple_parameter, $.required_parameter),
        alias($.optional_tuple_parameter, $.optional_parameter),
        $.optional_type,
        $.rest_type,
        $.type,
      ),

      constructor_type: $ => prec.left(seq(
        'new',
        optional($.type_parameters),
        $.formal_parameters,
        '=>',
        $.type
      )),

      _primary_type: $ => choice(
        $.parenthesized_type,
        $.predefined_type,
        $._type_identifier,
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
        $.conditional_type,
      ),

      infer_type: $ => seq("infer", $._type_identifier),

      conditional_type: $ => prec.left(seq(
        field('left', $.type),
        'extends',
        field('right', $.type),
        '?',
        field('consequence', $.type),
        ':',
        field('alternative', $.type)
      )),

      generic_type: $ => prec('call', seq(
        choice(
          $._type_identifier,
          $.nested_type_identifier
        ),
        $.type_arguments
      )),

      type_predicate: $ => field('type', seq(
        choice($.identifier, $.this),
        'is',
        $.type
      )),

      type_predicate_annotation: $ => seq(
        seq(':', $.type_predicate)
      ),

      type_query: $ => prec.right(seq(
        'typeof',
        choice($.primary_expression, $.generic_type),
      )),

      index_type_query: $ => seq(
        'keyof',
        $._primary_type,
      ),

      lookup_type: $ => seq(
        $._primary_type,
        '[',
        $.type,
        ']'
      ),

      mapped_type_clause: $ => seq(
        $._type_identifier,
        'in',
        $.type,
      ),

      literal_type: $ => choice(
        alias($._number, $.unary_expression),
        $.number,
        $.string,
        $.true,
        $.false
      ),

      _number: $ => prec.left(1, seq(
        field('operator', choice('-', '+')),
        field('argument', $.number)
      )),

      existential_type: $ => '*',

      flow_maybe_type: $ => prec.right(seq( '?', $._primary_type)),

      parenthesized_type: $ => seq(
        '(', $.type, ')'
      ),

      predefined_type: $ => choice(
        'any',
        'number',
        'boolean',
        'string',
        'symbol',
        'void'
      ),

      type_arguments: $ => seq(
        '<', commaSep1($.type), optional(','), '>'
      ),

      call_signature_in_interface: $ => $.call_signature_, 

      interface_member: $ => choice(
        prec.dynamic(10, $.export_statement),
        field('property', $.property_signature),
        $.construct_signature,
        $.index_signature,
        field('method', choice(
          prec(10, $.method_signature), 
          prec(-10, $.call_signature_in_interface)
        ))
      ), 

      object_type: $ => seq(
        choice('{', '{|'),
        optional_with_placeholder('interface_member_list', seq(
          optional(choice(',', ';')),
          sepBy1(
            choice(',', $._semicolon),
            $.interface_member
          ),
          optional(choice(',', $._semicolon))
        )),
        choice('}', '|}')
      ),

      property_signature: $ => prec.right(10, seq(
        optional_with_placeholder('modifier_list', seq(
          optional($.accessibility_modifier),
          optional($.static_modifier),
          optional($.readonly_modifier)
        )),
        field('assignment_variable', $.property_name),
        optional('?'),
        optional_with_placeholder('type_optional', $.type_annotation), 
        optional(';'))
      ),

      call_signature_: $ => seq(
        optional_with_placeholder('type_parameters', $.type_parameter), 
        field('parameters', $.formal_parameters),
        optional_with_placeholder('return_type_optional',
          choice($.type_annotation, $.asserts, $.type_predicate_annotation)
        )
      ),

      type_parameters: $ => seq(
        '<', commaSep1($.type_parameter), optional(','), '>'
      ),

      type_parameter: $ => seq(
        $._type_identifier,
        optional($.constraint),
        optional($.default_type)
      ),

      default_type: $ => seq(
        '=',
        $.type
      ),

      constraint: $ => seq(
        choice('extends', ':'),
        $.type
      ),

      construct_signature: $ => seq(
        'new',
        optional($.type_parameters),
        $.formal_parameters,
        optional($.type_annotation)
      ),

      index_signature: $ => seq(
        optional(
          seq(
            field("sign", optional("-")),
            $.readonly_modifier
          )
        ),
        '[',
        choice(
          seq(
            $.identifier_or_reserved_identifier,
            ':',
            $.type,
          ),
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
      tuple_type: $ => seq(
        '[', commaSep($._tuple_type_member), optional(','), ']'
      ),
      readonly_type: $ => seq($.readonly_modifier, $.type),

      union_type: $ => prec.left(seq(optional($.type), '|', $.type)),
      intersection_type: $ => prec.left(seq(optional($.type), '&', $.type)),

      function_type: $ => prec.left(seq(
        optional($.type_parameters),
        $.formal_parameters,
        '=>',
        choice($.type, $.type_predicate),
      )),

      _type_identifier: $ => alias($.identifier, $.type_identifier),

      _reserved_identifier: ($, previous) => choice(
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
  });
}

function commaSep1 (rule) {
  return sepBy1(',', rule);
}

function commaSep (rule) {
  return sepBy(',', rule);
}

function sepBy (sep, rule) {
  return optional(sepBy1(sep, rule))
}

function sepBy1 (sep, rule) {
  return seq(rule, repeat(seq(sep, rule)));
}

function optional_with_placeholder(field_name, rule) {
  return choice(field(field_name, rule), field(field_name, blank()));
}

// function alias()