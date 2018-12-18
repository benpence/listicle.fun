<html>
  <head>
    <title>listicle.fun</title>
    <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Patrick+Hand" />
    <link rel="stylesheet" href="/static/css/boxes.css" />
  </head>
  <body>
    <header>
      <span id="title">listicle.fun</span>
      <nav>
        <a class="menu" href="/">Next</a>
        <a class="menu" href="/">About</a>
      </nav>
    </header>

    <div id="content">
      <div id="stories">
        {{#mainStories}}
        <div class="story">
          <div class="story-text">
            <p class="story-headline" >{{headline}}</p>
            <p class="story-summary">{{summary}}</p>
            <p class="story-author">
              <span class="story-author-name">by {{author}}</span>
            </p>
          </div>
          <img class="story-image" src="{{imagePath}}" />
        </div>
        {{/mainStories}}
      </div>
    </div>

    <footer>
    </footer>
  </body>
</html>
