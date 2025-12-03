package pl.gamilife.grouptask.util;

import jakarta.persistence.criteria.Predicate;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import pl.gamilife.grouptask.entity.GroupTask;

import java.util.UUID;

@Component
public class GroupTasksSpecificationBuilderImpl implements GroupTasksSpecificationBuilder {


    @Override
    public Specification<GroupTask> build(UUID groupId, Boolean isAccepted) {
        return Specification.allOf(
                isCompleted(isAccepted),
                currentGroup(groupId)
        );
    }

    @Override
    public Specification<GroupTask> isCompleted(Boolean isAccepted) {
        return ((root, query, criteriaBuilder) -> {
            if (isAccepted == null) {
                return criteriaBuilder.conjunction();
            }

            Predicate hasAcceptedDate = criteriaBuilder.isNotNull(root.get("acceptedDate"));
            Predicate noAcceptedDate = criteriaBuilder.isNull(root.get("acceptedDate"));

            if (isAccepted) {
                return hasAcceptedDate;
            } else {
                return noAcceptedDate;
            }

        });
    }

    @Override
    public Specification<GroupTask> currentGroup(UUID groupId) {
        return ((root, query, criteriaBuilder) -> {
            if (groupId == null) {
                return criteriaBuilder.conjunction();
            }
            return criteriaBuilder.equal(root.get("groupId"), groupId);
        });
    }
}
