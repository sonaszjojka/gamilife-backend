package pl.gamilife.group.util;

import org.springframework.data.jpa.domain.Specification;
import pl.gamilife.group.enums.GroupTypeEnum;
import pl.gamilife.group.model.Group;

import java.util.UUID;

public interface UserGroupsSpecificationBuilder {
    Specification<Group> buildSpecification(UUID userId, String joinCode, GroupTypeEnum groupType, String groupName);
}
