package pl.gamilife.group.util;

import pl.gamilife.group.enums.GroupTypeEnum;
import pl.gamilife.group.model.Group;
import org.springframework.data.jpa.domain.Specification;

import java.util.UUID;

public interface UserGroupsSpecificationBuilder {
    Specification<Group> buildSpecification(UUID userId, String joinCode, GroupTypeEnum groupType, String groupName);
}
