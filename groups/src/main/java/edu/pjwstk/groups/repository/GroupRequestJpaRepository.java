package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupRequest;
import edu.pjwstk.groups.model.GroupRequestStatus;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface GroupRequestJpaRepository extends JpaRepository<GroupRequest, UUID> {
    boolean existsByGroupRequestedAndUserIdAndGroupRequestStatus(Group group, UUID userId, GroupRequestStatus groupRequestStatus);

    Optional<GroupRequest> findByGroupRequestIdAndGroupRequested_GroupId(UUID groupRequestId, UUID groupId);
}
