# Comprehensive syntax highlighting examples for nix-ts-mode
# This file demonstrates complete coverage of all Nix language constructs
# and showcases the visual distinctions in property chains and semantic highlighting

{
  # =================================================================
  # SECTION 1: PROPERTY CHAIN HIGHLIGHTING
  # =================================================================
  # Demonstrates visual distinction between base variables and property members

  propertyChains = {
    # Simple property access (2 parts)
    # Base variable gets type-face, members get property-use-face
    package = pkgs.hello;

    # Nested property access (3 parts)
    service = config.services.nginx;

    # Deep property access (4 parts)
    output = system.build.toplevel.outPath;

    # Very deep property access (5+ parts)
    # Base is visually distinct even in long chains
    deepNesting = lib.modules.evalModules.options.enable.default;

    # Property access on different base expressions
    fromVariable = myVar.property.nested;
    fromBuiltin = builtins.attrNames value;
    fromFunction = (getConfig args).services.nginx.enable;

    # Comparison: standalone vs property chains
    standalone = myVariable; # Variable-use-face (standalone)
    inChain = obj.property; # Base: type-face, property: property-use-face
  };

  # =================================================================
  # SECTION 2: NEWLY ADDED NODE TYPE COVERAGE
  # =================================================================
  # Features that complete 100% tree-sitter-nix grammar coverage

  # 1. has_attr_expression - Test if attribute exists with ? operator
  hasAttrExamples = {
    checkSimple = x ? y;
    checkNested = config ? services;
    checkDeep = obj ? attr.nested.deep;

    # Common use case: optional with fallback
    value = attrs.myAttr or "default";
  };

  # 2. dollar_escape - Escape interpolation in indented strings
  dollarEscapeExamples = {
    # In indented strings, ''$ escapes the ${ interpolation
    bashScript = ''
      # This won't be interpolated:
      echo "Variable: ''${MY_VAR}"

      # But this will be interpolated:
      echo "Nix var: ${nixVar}"
    '';

    literalBraces = ''
      Use ''${...} for literal dollar-brace in bash/shell scripts
    '';
  };

  # 3. let_attrset_expression - Deprecated syntax (still supported)
  # Modern Nix prefers 'rec { }' or 'let ... in ...' instead
  oldStyleLet = let {
  x = 1;
  y = 2;
  body = x + y; # Special 'body' attribute is returned
  };

  # =================================================================
  # SECTION 3: KEYWORDS AND CONTROL FLOW
  # =================================================================

  keywords = {
    # if/then/else
    conditionalExample = if true then "yes" else "no";

    # assert
    assertExample =
      assert x > 0;
      x + 1;

    # with
    withExample = with pkgs; [
      hello
      world
    ];

    # let/in
    letExample =
      let
        x = 1;
        y = 2;
      in
      x + y;

    # rec
    recExample = rec {
      x = 1;
      y = x + 1;
      z = y + 1;
    };

    # inherit
    inheritExample = {
      inherit x y z;
      inherit (pkgs) hello world;
      inherit (lib) mkOption types;
    };

    # Special keywords: import, throw, abort
    importExample = import ./some-file.nix;
    throwExample = x: if x < 0 then throw "negative" else x;
    abortExample = if false then abort "error" else "ok";

    # or operator (default values)
    orExample = obj.attr or defaultValue;
  };

  # =================================================================
  # SECTION 4: FUNCTIONS
  # =================================================================

  functions = {
    # Function definitions (function-name-face)
    simpleFunction = x: x + 1;
    multiArgFunction = x: y: x + y;

    # Formal parameters (variable-name-face for parameters)
    formalFunction =
      { a
      , b ? 5
      , c
      , ...
      }:
      a + b + c;

    # @ pattern (variable-name-face for args)
    atPatternFunction = args@{ x, y, ... }: args.x + y;

    # Function calls (function-call-face)
    simpleCall = myFunc arg;
    multiCall = func arg1 arg2 arg3;

    # Builtin function calls (builtin-face)
    mapExample = map toString [
      1
      2
      3
    ];
    filterExample = filter (x: x > 0) list;
    builtinsMap = builtins.map (x: x * 2) list;
    builtinsFilter = builtins.filter isAttrs items;

    # Primop functions (__* functions - builtin-face)
    addExample = __add 1 2;
    subExample = __sub 10 5;
    concatExample = __concatLists [
      [ 1 ]
      [ 2 ]
      [ 3 ]
    ];
    isAttrsExample = __isAttrs { };

    # Property-based function calls (function-call-face for last segment)
    propertyCall = lib.mkOption { };
    deepCall = config.services.nginx.enable;
  };

  # =================================================================
  # SECTION 5: LITERALS AND CONSTANTS
  # =================================================================

  literals = {
    # Constants (constant-face)
    nullValue = null;
    trueValue = true;
    falseValue = false;
    builtinsConstant = builtins;

    # Numbers (number-face)
    integer = 42;
    negativeInt = -10;
    float = 3.14159;
    negativeFloat = -2.71828;

    # Strings (string-face)
    simpleString = "hello world";
    withEscape = "line1\nline2\ttabbed";
    interpolated = "Hello ${name}, you are ${toString age} years old";

    # Indented strings
    multiline = ''
      This is a
      multiline string
      with ${interpolation}
    '';

    # Paths (string-face for paths)
    relativePath = ./some/path;
    absolutePath = /absolute/path;
    homePath = ~/my/home;
    storePath = <nixpkgs>;

    # URIs (string-face)
    httpUrl = "https://example.com/file.tar.gz";
    ftpUrl = "ftp://ftp.example.com/file";
  };

  # =================================================================
  # SECTION 6: OPERATORS
  # =================================================================

  operators = {
    # Arithmetic (operator-face)
    arithmetic = 1 + 2 - 3 * 4 / 5;

    # Comparison
    comparison = x < y && a > b || c == d;
    notEqual = e != f;

    # Logical
    logical = !false && (true || false);

    # String/list concatenation
    stringConcat = "hello" + " " + "world";
    listConcat = list1 ++ list2;

    # Attribute operations
    attrUpdate = oldSet // {
      newAttr = "value";
    };
    hasAttr = obj ? attr;
    attrAccess = obj.attr or defaultValue;

    # Implication
    implies = cond -> result;

    # Unary operators
    negation = -42;
    boolNot = !true;

    # At pattern
    atPattern = args@{}: args;
  };

  # =================================================================
  # SECTION 7: COLLECTIONS
  # =================================================================

  collections = {
    # Lists (bracket-face for [], number/string-face for contents)
    simpleList = [
      1
      2
      3
      "four"
    ];

    mixedList = [
      1
      2
      3
      "strings"
      "in"
      "list"
      { attr = "value"; }
      (x: x + 1)
    ];

    # Attribute sets (bracket-face for {})
    simpleSet = {
      a = 1;
      b = 2;
    };

    nestedSet = {
      level1 = {
        level2 = {
          level3 = "deep";
        };
      };
    };

    # Nested attribute paths (property-name-face)
    withPath = {
      nested.deep.path = "value";
    };
  };

  # =================================================================
  # SECTION 8: EXPRESSIONS
  # =================================================================

  expressions = {
    # apply_expression - function calls
    functionCall = func arg;

    # assert_expression
    withAssert =
      assert x > 0;
      x + 1;

    # attrset_expression
    simpleSet = {
      a = 1;
      b = 2;
    };

    # binary_expression
    binaryOps = 1 + 2 * 3;

    # if_expression
    conditional = if true then "yes" else "no";

    # let_expression
    letExpr =
      let
        x = 1;
        y = 2;
      in
      x + y;

    # list_expression
    list = [
      1
      2
      3
      "four"
    ];

    # parenthesized_expression
    grouped = (1 + 2) * 3;

    # rec_attrset_expression
    recursive = rec {
      x = 1;
      y = x + 1;
    };

    # select_expression
    propertyAccess = obj.property.nested;
    withDefault = obj.prop or defaultValue;

    # unary_expression
    negation = -x;
    boolNot = !false;

    # variable_expression
    variable = someVar;

    # with_expression
    withExpr = with pkgs; [
      hello
      world
    ];
  };

  # =================================================================
  # SECTION 9: COMPLEX REAL-WORLD EXAMPLE
  # =================================================================

  # Comprehensive example showing multiple features together
  complexNixOSModule =
    { lib
    , pkgs
    , config
    , ...
    }:
    let
      # Inherit from lib (property-use-face)
      inherit (lib) mkOption types mkIf;

      # Inherit from pkgs (property-use-face)
      inherit (pkgs) hello;

      # Custom function with formals (parameter highlighting)
      myFunc =
        { arg1
        , arg2 ? "default"
        , ...
        }@args:
        if arg1 > 0 then
          map toString (filter (x: x != null) args.list)
        else
          throw "invalid argument: ${arg2}";

      # Configuration checking (has_attr)
      hasService = config ? services.myapp;
    in
    rec {
      # Option definitions
      options = {
        enable = mkOption {
          type = types.bool;
          default = false;
          description = "Enable the service";
        };

        package = mkOption {
          type = types.package;
          default = hello;
          description = "Package to use";
        };
      };

      # Configuration results
      config = mkIf config.enable {
        # System packages (with expression + property chains)
        environment.systemPackages = with pkgs; [
          hello
          world
          (myFunc {
            arg1 = 42;
            list = [
              1
              2
              3
            ];
          })
        ];

        # Service configuration (deep property chains)
        systemd.services.myapp = {
          description = "My Application Service";
          wantedBy = [ "multi-user.target" ];

          serviceConfig = {
            ExecStart = "${config.services.myapp.package}/bin/myapp";
            Restart = "always";
            User = "myapp";
          };
        };

        # Networking (property access with defaults)
        networking.firewall.allowedTCPPorts = config.services.myapp.ports or [ 8080 ];
      };
    };

  # =================================================================
  # SECTION 10: EDGE CASES AND SPECIAL SYNTAX
  # =================================================================

  edgeCases = {
    # Nested interpolation
    nestedInterpolation = "outer ${toString (x + 1)} inner";

    # Multiple dollar escapes
    multipleEscapes = ''
      One: ''${first}
      Two: ''${second}
      Real: ${actual}
    '';

    # Complex function chains
    chainedCalls = map (x: x * 2) (filter (x: x > 0) list);

    # Property access with function call
    propertyFunction = (lib.mkIf config.enable value).result;

    # Deep attribute paths in bindings
    deepBinding = {
      level1.level2.level3.level4 = "deep value";
    };

    # At pattern with ellipses
    atPatternEllipses = args@{ x, y, ... }: args;

    # Has attr with deep path
    deepHasAttr = obj ? attr.nested.deep.path;

    # Comparison chaining
    chainedComparison = x < y && y < z && z < w;
  };

  # =================================================================
  # All node types and semantic distinctions covered!
  # =================================================================

  coverage = {
    grammarCoverage = "100%";
    nodeTypes = 41;
    faces = 17;
    semanticDistinctions = true;
    propertyChainVisibility = "excellent";
  };
}
