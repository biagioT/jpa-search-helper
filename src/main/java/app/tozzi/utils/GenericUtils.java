package app.tozzi.utils;

import java.text.DecimalFormat;
import java.text.ParseException;

public class GenericUtils {
    public static Number formatNumber(Number decimalNumber, String pattern, boolean bigDecimal) throws ParseException {
        DecimalFormat df = new DecimalFormat(pattern);
        df.setParseBigDecimal(bigDecimal);
        return df.parse(df.format(decimalNumber));
    }

    public static boolean parseBoolean(String value) {

        if ("true".equalsIgnoreCase(value)) {
            return true;
        }

        if ("false".equalsIgnoreCase(value)) {
            return false;
        }

        throw new IllegalArgumentException();
    }

}
