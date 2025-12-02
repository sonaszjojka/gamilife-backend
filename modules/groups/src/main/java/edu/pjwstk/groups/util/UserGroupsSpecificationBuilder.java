package edu.pjwstk.groups.util;

import edu.pjwstk.groups.enums.GroupTypeEnum;
import edu.pjwstk.groups.model.Group;
import org.springframework.data.jpa.domain.Specification;

import java.util.UUID;

public interface UserGroupsSpecificationBuilder {
    Specification<Group> buildSpecification(UUID userId, String joinCode, GroupTypeEnum groupType, String groupName);
}
