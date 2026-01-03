package pl.gamilife.group.util.impl;

import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import pl.gamilife.group.enums.GroupTypeEnum;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.model.GroupType;
import pl.gamilife.group.util.UserGroupsSpecificationBuilder;

import java.util.UUID;

@Component
public class UserGroupsSpecificationBuilderImpl implements UserGroupsSpecificationBuilder {

    @Override
    public Specification<Group> buildSpecification(
            UUID userId,
            GroupTypeEnum groupType,
            String groupName
    ) {
        return Specification.allOf(
                belongsToUser(userId),
                hasGroupType(groupType),
                hasGroupName(groupName)
        );
    }

    private Specification<Group> belongsToUser(UUID userId) {
        return (root, query, cb) -> {
            if (userId == null) {
                return null;
            }
            Join<Group, GroupMember> gmJoin = root.join("activeMembers", JoinType.INNER);
            query.distinct(true);
            return cb.and(
                    cb.equal(gmJoin.get("userId"), userId),
                    cb.isNull(gmJoin.get("leftAt"))
            );
        };
    }

    private Specification<Group> hasGroupType(GroupTypeEnum groupType) {
        return (root, query, cb) -> {
            if (groupType == null) {
                return null;
            }
            Join<Group, GroupType> groupTypeJoin = root.join("groupType");
            return cb.equal(groupTypeJoin.get("id"), groupType.getId());
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
