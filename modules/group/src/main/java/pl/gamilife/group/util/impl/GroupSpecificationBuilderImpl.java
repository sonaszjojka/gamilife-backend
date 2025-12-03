package pl.gamilife.group.util.impl;

import jakarta.persistence.criteria.Join;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import pl.gamilife.group.enums.GroupTypeEnum;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.util.GroupSpecificationBuilder;


@Component
public class GroupSpecificationBuilderImpl implements GroupSpecificationBuilder {

    @Override
    public Specification<Group> buildSpecification(
            String joinCode,
            GroupTypeEnum groupType,
            String groupName) {

        return Specification.allOf(
                hasJoinCode(joinCode),
                hasGroupType(groupType),
                hasGroupName(groupName)
        );
    }

    private Specification<Group> hasJoinCode(String joinCode) {
        return (root, query, cb) -> {
            if (joinCode == null || joinCode.isBlank()) {
                return null;
            }
            return cb.equal(root.get("joinCode"), joinCode.trim());
        };
    }

    private Specification<Group> hasGroupType(GroupTypeEnum groupType) {
        return (root, query, cb) -> {
            if (groupType == null) {
                return null;
            }
            Join<Object, Object> groupTypeJoin = root.join("groupType");
            return cb.equal(groupTypeJoin.get("groupTypeId"), groupType.getId());
        };
    }

    private Specification<Group> hasGroupName(String groupName) {
        return (root, query, cb) -> {
            if (groupName == null || groupName.isBlank()) {
                return null;
            }
            String searchPattern = "%" + groupName.trim().toLowerCase() + "%";
            return cb.like(cb.lower(root.get("name")), searchPattern);
        };
    }
}
