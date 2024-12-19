Shiny.addCustomMessageHandler("update_pop_model_sr_inputs", function(data) {
  console.log("Received data from R:", data);

  // Prefix and suffix for targeting specific inputs
  const prefix = "matrix_model-mm_preview-";
  const numericSuffixes = ["-pm_ps_val_mean", "-pm_ps_val_sd", "-pm_ps_val_lwr", "-pm_ps_val_upr"];
  const checkboxSuffix = "-pm_ps_check";

  // Uncheck all checkboxes with the specified prefix and suffix
  const allCheckboxes = document.querySelectorAll(`input[type="checkbox"][id^="${prefix}"][id$="${checkboxSuffix}"]`);
  allCheckboxes.forEach(function(checkbox) {
    checkbox.checked = false;
    checkbox.dispatchEvent(new Event("change")); // Notify Shiny of the change
  });

  // Clear all numeric inputs with the specified prefix and suffixes
  numericSuffixes.forEach(function(suffix) {
    const numericInputs = document.querySelectorAll(`input[type="number"][id^="${prefix}"][id$="${suffix}"]`);
    numericInputs.forEach(function(input) {
      input.value = ""; // Clear the value
      input.dispatchEvent(new Event("input")); // Notify Shiny of the change
      input.dispatchEvent(new Event("change")); // Notify Shiny of the change
    });
  });

  // Now handle the incoming data to update specific inputs
  Object.keys(data).forEach(function(variable) {
    const values = data[variable];

    // Generate IDs dynamically
    const meanId = prefix + variable + "-pm_ps_val_mean";
    const sdId = prefix + variable + "-pm_ps_val_sd";
    const lwrId = prefix + variable + "-pm_ps_val_lwr";
    const uprId = prefix + variable + "-pm_ps_val_upr";
    const checkId = prefix + variable + "-pm_ps_check";

    // Function to update text/numeric input and trigger events
    const updateInput = function(id, value) {
      const element = document.getElementById(id);
      if (element) {
        element.value = value; // Update the value
        element.dispatchEvent(new Event("input")); // Trigger 'input' event
        element.dispatchEvent(new Event("change")); // Trigger 'change' event
      }
    };

    // Function to update checkbox input and trigger events
    const updateCheckbox = function(id, checked) {
      const element = document.getElementById(id);
      if (element) {
        element.checked = checked; // Update the checkbox state
        element.dispatchEvent(new Event("change")); // Trigger 'change' event
      }
    };

    // Update each numeric input
    updateInput(meanId, values.mean);
    updateInput(sdId, values.sd);
    updateInput(lwrId, values.lwr);
    updateInput(uprId, values.upr);

    // Update checkbox if "check" is provided
    if (typeof values.check !== "undefined") {
      updateCheckbox(checkId, values.check);
    }
  });
});
