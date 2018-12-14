<html>
    <head>
        <title>listicle.fun</title>
        <link rel="stylesheet" href="/static/css/style.css"></link>
    </head>
    <body>
        <h1>listicle.fun</h1>
		{{#stories}}
		<div>
			<img src="{{image}}" />
			<a href="#">{{title}}</a>
		</div>
		{{/stories}}
    </body>
    <footer>
    </footer>
</html>
