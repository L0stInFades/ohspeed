#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>

#ifdef _WIN32
#include <conio.h>
#include <stdint.h>
#include <stdio.h>
#include <windows.h>

static void raise_last_error(const char *operation)
{
  char message[160];
  snprintf(message, sizeof(message), "%s failed with Windows error %lu",
           operation, (unsigned long)GetLastError());
  caml_failwith(message);
}

static HANDLE console_handle(value output_v)
{
  DWORD stream = Bool_val(output_v) ? STD_OUTPUT_HANDLE : STD_INPUT_HANDLE;
  HANDLE handle = GetStdHandle(stream);
  if (handle == INVALID_HANDLE_VALUE || handle == NULL) {
    raise_last_error("GetStdHandle");
  }
  return handle;
}

CAMLprim value ohspeed_win32_get_console_mode(value output_v)
{
  DWORD mode = 0;
  if (!GetConsoleMode(console_handle(output_v), &mode)) {
    raise_last_error("GetConsoleMode");
  }
  return caml_copy_int32((int32_t)mode);
}

CAMLprim value ohspeed_win32_set_console_mode(value output_v, value mode_v)
{
  DWORD mode = (DWORD)Int32_val(mode_v);
  if (!SetConsoleMode(console_handle(output_v), mode)) {
    raise_last_error("SetConsoleMode");
  }
  return Val_unit;
}

CAMLprim value ohspeed_win32_kbhit(value unit_v)
{
  (void)unit_v;
  return Val_bool(_kbhit() != 0);
}

CAMLprim value ohspeed_win32_getch(value unit_v)
{
  (void)unit_v;
  return Val_int(_getch());
}

#else

CAMLprim value ohspeed_win32_get_console_mode(value output_v)
{
  (void)output_v;
  caml_failwith("Win32 console mode is only available on Windows");
}

CAMLprim value ohspeed_win32_set_console_mode(value output_v, value mode_v)
{
  (void)output_v;
  (void)mode_v;
  caml_failwith("Win32 console mode is only available on Windows");
}

CAMLprim value ohspeed_win32_kbhit(value unit_v)
{
  (void)unit_v;
  caml_failwith("Win32 keyboard polling is only available on Windows");
}

CAMLprim value ohspeed_win32_getch(value unit_v)
{
  (void)unit_v;
  caml_failwith("Win32 keyboard input is only available on Windows");
}

#endif
