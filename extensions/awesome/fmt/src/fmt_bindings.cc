/*
 * Example clbind bindings for fmt library
 * This is a minimal hand-crafted example to demonstrate the structure
 * Use generate_bindings.py to create full bindings
 */

#include <clasp/clbind/clbind.h>
#include <string>
#include <stdexcept>

// For this example, we'll create wrapper functions that simulate fmt
// In the real generated version, we'd include <fmt/format.h>

namespace fmt_example {

// Simplified format function for demonstration
std::string format_demo(const std::string& fmt_str, const std::string& arg) {
    // Simple string substitution for demo
    std::string result = fmt_str;
    size_t pos = result.find("{}");
    if (pos != std::string::npos) {
        result.replace(pos, 2, arg);
    }
    return result;
}

class format_error : public std::runtime_error {
public:
    explicit format_error(const std::string& what_arg)
        : std::runtime_error(what_arg) {}
};

} // namespace fmt_example

namespace awesome_fmt_bindings {

using namespace clbind;

void initialize_fmt_bindings() {
  package_ pkg("AWESOME-FMT");
  scope_& scope = pkg.scope();

  // Expose format_error exception
  class_<fmt_example::format_error, std::runtime_error>(scope, "format-error")
    .def_constructor<const std::string&>();

  // Expose format function (demo version)
  scope.def("format-demo", &fmt_example::format_demo);

  // Note: In the full generated version, we would expose the real fmt::format
  // and all other fmt functions/classes
}

} // namespace awesome_fmt_bindings

// Clasp initialization entry point
// This function will be called when the extension is loaded
extern "C" {
#ifdef __cplusplus
extern "C" {
#endif

void initialize_awesome_fmt() {
  awesome_fmt_bindings::initialize_fmt_bindings();
}

#ifdef __cplusplus
}
#endif
}
