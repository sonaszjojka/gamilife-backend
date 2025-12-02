package pl.gamilife.group.util;

import pl.gamilife.group.enums.GroupTypeEnum;
import pl.gamilife.group.model.Group;
import org.springframework.data.jpa.domain.Specification;

public interface GroupSpecificationBuilder {
    Specification<Group> buildSpecification(String joinCode, GroupTypeEnum groupType, String groupName);
}