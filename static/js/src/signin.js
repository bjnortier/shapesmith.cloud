(function() {
    var submit = function() {
	var username = $('#username').val().trim();
	var password = $('#password').val().trim();
	var postJson = {username: username,
			password: password};
	$('#spinner-container').append('<div id="spinner"><img src="/static/images/progress-spinner.gif" alt="in progress"/></div>');
	$.ajax({
	    type: 'POST',
	    url: '/signin/',
	    data: JSON.stringify(postJson),
	    dataType: 'json',
	    contentType: 'application/json',
	    success: function(response) {
		window.location.href = '/' + username + '/designs';
	    },
	    error: function(response) {
		SS.render_errors(JSON.parse(response.responseText));
		$('#spinner').remove();
	    }
	});
    }
    $('#password').keypress(function(event) {
	if (event.keyCode === 13) {	
	    submit();
	    return false;	
	}
    })
    $('#username').keypress(function(event) {
	if (event.keyCode === 13) {	
	    submit();
	    return false;	
	}
    })
    $('#signin-button').click(function() {
	submit();
	return false;
    });

    $('#temp-account-button').click(function() {
        $('#spinner-container').append('<div id="spinner"><img src="/static/images/progress-spinner.gif" alt="in progress"/></div>');
        $.ajax({
            type: 'POST',
            url: '/temp_user/',
            data: JSON.stringify({}),
            dataType: 'json',
            contentType: 'application/json',
            success: function(response) {
                var username = response.username;
                window.location.href = '/' + username + '/designs';
            },
            error: function(response) {
                SS.render_errors(JSON.parse(response.responseText));
                $('#spinner').remove();
            }
        });
        return false;
    });
})();



