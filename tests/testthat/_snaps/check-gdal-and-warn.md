# check gdal works

    Code
      check_gdal_and_warn(maj_v_min = majv + 1, min_v_min = 0, patch_v_min = 0)
    Message
      ! You are using GDAL version 3.11.3 which is not compliant
        with the minimum recommended version 4.0.0.
        Please update GDAL to a newer version to ensure compatibility with vrtility.

---

    Code
      check_gdal_and_warn(maj_v_min = majv, min_v_min = minv + 1, patch_v_min = 0)
    Message
      ! You are using GDAL version 3.11.3 which is not compliant
        with the minimum recommended version 3.12.0.
        Please update GDAL to a newer version to ensure compatibility with vrtility.

---

    Code
      check_gdal_and_warn(maj_v_min = majv, min_v_min = minv, patch_v_min = patchv +
      1)
    Message
      ! You are using GDAL version 3.11.3 which is not compliant
        with the minimum recommended version 3.11.4.
        Please update GDAL to a newer version to ensure compatibility with vrtility.

---

    Code
      check_gdal_and_warn(maj_v_min = majv, min_v_min = minv, patch_v_min = patchv)
    Message
      v Using GDAL version 3.11.3

---

    Code
      check_gdal_and_warn(maj_v_min = 0, min_v_min = 0, patch_v_min = 1)
    Message
      v Using GDAL version 3.11.3

