package app.tozzi.model;

import app.tozzi.model.input.JPASearchInput;
import jakarta.persistence.Tuple;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import jakarta.persistence.criteria.Selection;
import lombok.AllArgsConstructor;
import lombok.Data;

import java.util.List;

@Data
@AllArgsConstructor
public class ProjectionDescriptor {

    private CriteriaQuery<Tuple> criteriaQuery;
    private List<Selection<?>> selections;
    private JPASearchInput input;
    private Root<?> root;

}
