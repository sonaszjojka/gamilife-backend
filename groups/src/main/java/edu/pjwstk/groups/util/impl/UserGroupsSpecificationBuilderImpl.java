package edu.pjwstk.groups.util.impl;

import edu.pjwstk.groups.enums.GroupTypeEnum;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;
import edu.pjwstk.groups.model.GroupType;
import edu.pjwstk.groups.util.UserGroupsSpecificationBuilder;
import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class UserGroupsSpecificationBuilderImpl implements UserGroupsSpecificationBuilder {

    @Override
    public Specification<Group> buildSpecification(
            UUID userId,
            String joinCode,
            GroupTypeEnum groupType,
            String groupName
    ) {
        return Specification.allOf(
                belongsToUser(userId),
                hasJoinCode(joinCode),
                hasGroupType(groupType),
                hasGroupName(groupName)
        );
    }

    private Specification<Group> belongsToUser(UUID userId) {
        return (root, query, cb) -> {
            if (userId == null) {
                return null;
            }
            Join<Group, GroupMember> gmJoin = root.join("groupMembers", JoinType.INNER);
            query.distinct(true);
            return cb.and(
                    cb.equal(gmJoin.get("userId"), userId),
                    cb.isNull(gmJoin.get("leftAt"))
            );
        };
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
            Join<Group, GroupType> groupTypeJoin = root.join("groupType");
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
