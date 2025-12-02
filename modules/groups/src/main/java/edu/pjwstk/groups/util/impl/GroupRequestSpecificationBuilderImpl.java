package edu.pjwstk.groups.util.impl;

import edu.pjwstk.groups.enums.GroupRequestStatusEnum;
import edu.pjwstk.groups.model.GroupRequest;
import edu.pjwstk.groups.util.GroupRequestSpecificationBuilder;
import jakarta.persistence.criteria.Join;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class GroupRequestSpecificationBuilderImpl implements GroupRequestSpecificationBuilder {

    @Override
    public Specification<GroupRequest> buildSpecification(
            UUID groupId,
            GroupRequestStatusEnum statusEnum) {

        return Specification.allOf(
                hasGroupId(groupId),
                hasStatus(statusEnum)
        );
    }

    private Specification<GroupRequest> hasGroupId(UUID groupId) {
        return (root, query, cb) -> {
            if (groupId == null) {
                return null;
            }
            return cb.equal(root.get("groupId"), groupId);
        };
    }

    private Specification<GroupRequest> hasStatus(GroupRequestStatusEnum statusEnum) {
        return (root, query, cb) -> {
            if (statusEnum == null) {
                return null;
            }
            Join<Object, Object> statusJoin = root.join("groupRequestStatus");
            return cb.equal(statusJoin.get("groupRequestStatusId"), statusEnum.getId());
        };
    }
}