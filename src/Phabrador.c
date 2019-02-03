#include <fluid_shared.h>
#include <CoreImage/CoreImage.h>

CAMLprim value phabrador_homeDirectory() {
  CAMLparam0();
  CAMLreturn(caml_copy_string([NSHomeDirectory() UTF8String]));
}

void phabrador_setTimeout(value callback, value milis_v) {
  CAMLparam2(callback, milis_v);
  int64_t milis = Int_val(milis_v);
  int cbid = Int_val(callback);
  int64_t nanos = milis * 1000 * 1000;
  // printf("Waiting for %d milis, %f nanos\n", milis, nanos);
  dispatch_after(dispatch_time(DISPATCH_TIME_NOW, nanos), dispatch_get_main_queue(), ^{
    CAMLparam0();

    static value * closure_f = NULL;
    if (closure_f == NULL) {
        /* First time around, look up by name */
        closure_f = caml_named_value("phabrador_timeout_cb");
    }
    caml_callback2(*closure_f, Val_int(cbid), Val_unit);

    CAMLreturn0;
  });
  CAMLreturn0;
}

void phabrador_openUrl(value url) {
  CAMLparam1(url);
  [[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString: NSString_val(url)]];
  CAMLreturn0;
}

void qmenu_toggleMenuItem(value item_v, value isOn) {
  CAMLparam1(item_v);
  NSMenuItem* item = (NSMenuItem*)Unwrap(item_v);
  [item setState:isOn == Val_true ? NSControlStateValueOn : NSControlStateValueOff];
  // [item setState:NSControlStateValueOn];
  CAMLreturn0;
}

void phabrador_showMenu(value menu_v) {
  CAMLparam1(menu_v);
  NSMenu* menu = (NSMenu*)Unwrap(menu_v);
  [NSMenu popUpContextMenu:menu withEvent:[NSApp currentEvent] forView:nil];
  CAMLreturn0;
}

void phabrador_fetch(value url, value callback, value headers) {
  CAMLparam1(url);
  int callbackId = Int_val(callback);
  NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:[[NSURL alloc] initWithString:NSString_val(url)]];
  int length = Wosize_val(headers);
  for (int i=0; i<length; i++) {
    value name_v = Field(Field(headers, i), 0);
    NSString* name = NSString_val(name_v);
    value value_v = Field(Field(headers, i), 1);
    NSString* value = NSString_val(value_v);
    [request setValue:value forHTTPHeaderField:name];
  }
  NSURLSessionDataTask* task = [[NSURLSession sharedSession]
      dataTaskWithRequest:request
    completionHandler:^(NSData* data, NSURLResponse *response, NSError *error){
      NSHTTPURLResponse* httpResponse = (NSHTTPURLResponse*)response;
      NSString *text = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
      dispatch_async(dispatch_get_main_queue(), ^{
        CAMLparam0();
        CAMLlocal1(tuple_v);

        static value * closure_f = NULL;
        if (closure_f == NULL) {
            /* First time around, look up by name */
            closure_f = caml_named_value("phabrador_fetch_cb");
        }

        tuple_v = caml_alloc_tuple(2);
        Store_field(tuple_v, 0, caml_copy_string([text UTF8String]));
        Store_field(tuple_v, 1, Val_int(httpResponse.statusCode));

        caml_callback2(*closure_f, Val_int(callbackId), tuple_v);
        CAMLreturn0;
      });
    }];
  [task resume];
  CAMLreturn0;
}