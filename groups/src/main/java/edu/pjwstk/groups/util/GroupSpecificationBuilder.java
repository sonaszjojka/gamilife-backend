package edu.pjwstk.groups.util;

import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.shared.GroupTypeEnum;
import org.springframework.data.jpa.domain.Specification;

public interface GroupSpecificationBuilder {
    Specification<Group> buildSpecification(String joinCode, GroupTypeEnum groupType, String groupName);
}