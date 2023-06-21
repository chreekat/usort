<svelte:head>
  <title>USort</title>
</svelte:head>

<script>
</script>

<h1 class="title is-1">
  USort!
</h1>

<section>
  <h2 class="title is-5">Which item is more important?</h2>
  <h3 class="subtitle is-6">Choice 3 of ~9</h3>
  <div class="tile is-ancestor">
    <div class="tile is-vertical is-parent">
      <div class="tile is-child">
        <div class="card">
          <header class="is-clickable card-header">
            <div class="card-header-title">1</div>
          </header>
          <div class="card-content">
            <textarea>This isoeuhaoe u8hoaeu a8oe huaoeu 8gao hueahoe u8aeoh
            ahoe uaoeua8ogueao8euaeo8u aoeuhaoeu h8aoeu 8 the first item.</textarea>
          </div>
          <footer class="card-footer">
            <a href="compare-view" class="button card-footer-item">Save</a>
            <a href="#" class="button card-footer-item">Ignore</a>
            <a href="#" class="button card-footer-item">Delete</a>
          </footer>
        </div>
      </div>
      <div class="tile is-child">
        <div class="card">
          <div class="card-header">
            <div class="card-header-title">2</div>
          </div>
          <div class="card-content">
            This is the second item.
          </div>
          <div class="card-footer">
            <a href="edit-view.html" class="button card-footer-item">Edit</a>
            <a href="#" class="button card-footer-item">Ignore</a>
            <a href="#" class="button card-footer-item">Delete</a>
          </div>
        </div>
      </div>
      <div class="tile is-child">
        <button class="button">Pick 1</button>
        <button class="button">Pick 2</button>
        <button class="button">Undo</button>
        <button class="button">Add new</button>
        <button class="button">Stop sorting</button>
      </div>
  </div>
</section>
