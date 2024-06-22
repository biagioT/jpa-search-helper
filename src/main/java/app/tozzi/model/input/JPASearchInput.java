package app.tozzi.model.input;

import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.List;

@Data
public class JPASearchInput {

    private RootFilter filter;
    private JPASearchOptions options;

    @Data
    public static class JPASearchOptions {
        private String sortKey;
        private Boolean sortDesc = false;
        private Integer pageSize;
        private Integer pageOffset;
    }

    @Data
    public static abstract class Filter {
        private String operator;
    }

    @Data
    @EqualsAndHashCode(callSuper = true)
    public static class RootFilter extends Filter {
        private List<Filter> filters;
    }

    @Data
    @EqualsAndHashCode(callSuper = true)
    public static class FieldFilter extends Filter {
        private String key;
        private JPASearchFilterOptions options;
    }

    @Data
    @EqualsAndHashCode(callSuper = true)
    public static class FilterSingleValue extends FieldFilter {
        private Object value;
    }

    @Data
    @EqualsAndHashCode(callSuper = true)
    public static class FilterMultipleValues extends FieldFilter {
        private List<Object> values;
    }

    @Data
    public static class JPASearchFilterOptions {
        private boolean ignoreCase;
        private boolean trim;
        private boolean negate;
    }

}
