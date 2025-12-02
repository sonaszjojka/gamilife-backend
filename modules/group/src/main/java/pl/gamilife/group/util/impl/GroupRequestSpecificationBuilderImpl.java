package pl.gamilife.group.util.impl;

import pl.gamilife.group.enums.GroupRequestStatusEnum;
import pl.gamilife.group.model.GroupRequest;
import pl.gamilife.group.util.GroupRequestSpecificationBuilder;
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