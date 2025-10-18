package edu.pjwstk.groups.repository.jpa;

import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupRequest;
import edu.pjwstk.groups.entity.GroupRequestStatus;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface GroupRequestRepositoryJpa extends JpaRepository<GroupRequest, UUID> {
    boolean existsByGroupRequestedAndUserIdAndGroupRequestStatus(Group group, UUID userId, GroupRequestStatus groupRequestStatus);

    Optional<GroupRequest> findByUserIdAndGroupRequestedAndGroupRequestStatus(UUID userId, Group groupRequested, GroupRequestStatus groupRequestStatus);

    Optional<GroupRequest> findByUserIdAndGroupRequestedAndGroupRequestStatus_GroupRequestStatusId(UUID userId, Group groupRequested, Integer groupRequestStatusGroupRequestStatusId);
}
