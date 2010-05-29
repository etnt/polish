$(document).ready(function() {
	var max_line_size = 60;
	$('textarea').focus(function() {
		var str = $(this).val();
		var rows = 0;
		var cols = 0;
		if(str.length > max_line_size) {
		    rows = Math.ceil(str.length / max_line_size) + 1;
		    cols = max_line_size;
		}
		else {
		    rows = 2;
		    cols = str.length;
		}

		$(this).attr('cols', cols);
		$(this).attr('rows', rows);
	    });

	$(".search").fancybox({
		'transitionIn'	:	'fade',
		'transitionOut'	:	'fade',
		'speedIn'	:	600,
		'speedOut'	:       200,
	});

	$(".statsbutton").fancybox({
		'transitionIn'	:	'fade',
		'transitionOut'	:	'fade',
		'speedIn'	:	600,
		'speedOut'	:       200,
	});

	var position = $("table").position();
	$(".button").each(function(i) {
	  if($(this).val() == "Next" || $(this).val() == "Write") {
            $(this).css('margin-left', 0);
            $(this).css('position', 'relative');
            $(this).css('left', position.left+'px');
          }
	});
});