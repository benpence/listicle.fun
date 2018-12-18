<html>
  <head>
    <title>listicle.fun</title>
    <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Yanone+Kaffeesatz" >
    <link rel="stylesheet" href="/static/css/basic.css" />
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
          <img class="story-image" src="{{imagePath}}" />
          <div class="story-text">
            <p class="story-headline" >{{headline}}</p>
            <p class="story-summary">{{summary}}</p>
            <p class="story-author">
              <span class="story-author-avatar">{{avatar}}</span>
              <span class="story-author-name">{{author}}</span>
            </p>
          </div>
        </div>
        {{/mainStories}}
      </div>

      <div id="sidebar">
        {{#sideStories}}
        <div class="sidebar-story">
          <img class="sidebar-image" src="{{imagePath}}" />
          <span class="sidebar-headline">{{headline}}</span>
        </div>
        {{/sideStories}}
      </div>
    </div>

    <footer>
    </footer>
  </body>
</html>
