function main_table_module_js(ns_prefix) {
  
  $("#" + ns_prefix + "main_table").on("click", ".delete_btn", function() {
    console.log(`Delete record... ${this.id}`);
    Shiny.setInputValue(ns_prefix + "main_id_to_delete", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
  
  $("#" + ns_prefix + "main_table").on("click", ".edit_btn", function() {
    console.log(`Edit record... ${this.id}`);
    Shiny.setInputValue(ns_prefix + "main_id_to_edit", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
}