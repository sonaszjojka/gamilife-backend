package pl.gamilife.group.util;

import org.springframework.data.jpa.domain.Specification;
import pl.gamilife.group.enums.GroupTypeEnum;
import pl.gamilife.group.model.Group;

public interface GroupSpecificationBuilder {
    Specification<Group> buildSpecification(GroupTypeEnum groupType, String groupName);
}