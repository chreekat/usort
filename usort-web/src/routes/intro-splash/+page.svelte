<script>
  let input = "some stuff";
  let lines = usortSplitItems(input);
</script>
<svelte:head>
  <title>USort</title>
</svelte:head>

<div class="content">
  <h1 class="title">
    USort!
  </h1>
  <p class="subtitle">
    With USort, the sort is you!
  </p>

  <section class="section">
    <p>
      Enter your list to be sorted. See examples below.
    </p>
    <textarea class="textarea" bind:value={input} blop="Item 1Item 2"></textarea>
    <div>
      <button class="button">One item per line</button>
      <button class="button">Split items on blank lines</button>
      <button class="button">Split items based on indent</button>
    </div>
    <input type="text" bind:value={input}>
  </section>
  <section class="section">
    <h2 class="title">Item preview</h2>
    <div>Item 1</div>
    <div>Item 2</div>
    <p>
      <a href="compare-view" class="button">Go!</a>
    </p>
  </section>
</div>
