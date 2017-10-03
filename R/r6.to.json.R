r6.to.json <- function( o, o.class = get(class(o)), o.fields = names(o.class$public_fields) ) {
  o.return = list();
  for(f in o.fields) { o.return[[f]] = o[[f]] }

  o.return

}
