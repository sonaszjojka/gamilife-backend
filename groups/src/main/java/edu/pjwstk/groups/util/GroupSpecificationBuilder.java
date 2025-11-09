package edu.pjwstk.groups.util;

import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.enums.GroupTypeEnum;
import org.springframework.data.jpa.domain.Specification;

public interface GroupSpecificationBuilder {
    Specification<Group> buildSpecification(String joinCode, GroupTypeEnum groupType, String groupName);
}