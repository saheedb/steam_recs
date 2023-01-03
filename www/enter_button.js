$(document).keyup(function(event) {
    if ($("#game_search").is(":focus") && (event.key == "Enter")) {
        $("#game_search_button").click();
    }
});