# Call the testing suite
diseasystore::test_diseasystore(
  diseasystore_generator = DiseasystoreSeirExample,
  conn_generator = get_test_conns,
  target_schema = target_schema_1,
  test_start_date = min(seir_example_data$date),
  slice_ts = Sys.Date()
)
