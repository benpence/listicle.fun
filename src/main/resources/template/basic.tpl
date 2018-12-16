<html>
    <head>
        <title>listicle.fun</title>
        <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Yanone+Kaffeesatz" >
        <link rel="stylesheet" href="/static/css/basic.css"></link>
    </head>
    <body>
      <header>
        
      </header>
      <div>
        <h1>listicle.fun</h1>

        <div id="content">
            <div id="stories">
                {{#mainStories}}
                <div class="story">
                    <img class="story-image" src="{{image}}" />
                    <span class="story-title" >{{title}}</span>
                </div>
                {{/mainStories}}
            </div>

            <div id="sidebar" class="box-top">
                {{#sideStories}}
                <div class="sidebar-story">
                    <img class="sidebar-image" src="{{image}}" />
                    <span class="sidebar-title">{{title}}</span>
                </div>
                {{/sideStories}}
            </div>
        </div>
        <footer>
        </footer>
    </body>
</html>
