/* TestFramework.re */
include Rely.Make({
  let config =
    Rely.TestFrameworkConfig.initialize({
      snapshotDir: "test/lib/__snapshots__",
      projectDir: "lib"
    });
});
