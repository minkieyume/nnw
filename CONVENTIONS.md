1. Coding Style
- Minimalism First: Write simple, clean, and idiomatic code. Avoid over-engineering.
- Clarity: Prioritize readability over cleverness.
- Human First: Code should be self-documenting. Use descriptive names, clear structure, 
  and comments only when logic is non-obvious. Optimize for maintainability, not brevity.

2. Language & Documentation
- Reply in Chinese: All responses regarding architecture and code enhancements must be in Chinese.
- Write in English : All code comments, documentation, and git commit messages must be written in English.

3. Testing
- Test Location: All unit tests must be placed in the /test directory.
- Requirement: Ensure new features include corresponding unit tests.
- Test Name：All unit test's test-begin or test-ends' names must begin with logs/ ,
  test-group test-error and other functions dosen't follow this rule.
- Test Conciseness：Test code must be minimized in line count. For each target function, test-group usage is strictly limited to a maximum of 3.
- Test Scope：Avoid over-testing implementation details. Focus strictly on the function's core capability (e.g., asserting fixed inputs against expected outputs).

4. Reference
- Documentation Priority: Always prioritize the official Guile Manual (https://www.gnu.org/software/guile/manual/guile.html) as the primary technical reference for syntax and built-in features.
- Third-party Modules: For any modules or libraries not covered in the official Guile documentation, you are exempt from the "Guile First" rule and should utilize the most relevant external documentation.

5. Error Codes
- When using `let-keywords`, all possible keyword arguments must be explicitly handled in the binding list, otherwise the code will fail to compile.

6. Other
- Automatic Imports: When generating code that depends on external modules, always include the necessary use-modules statements or relevant import declarations automatically.
- API Validation: Verify that the functions, macros, or variables being used are actually exported by and present within the referenced modules to prevent "unbound variable" or "undefined identifier" errors.
