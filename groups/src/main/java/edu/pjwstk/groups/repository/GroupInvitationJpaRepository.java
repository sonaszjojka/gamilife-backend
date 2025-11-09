package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupInvitation;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface GroupInvitationJpaRepository extends JpaRepository<GroupInvitation, UUID> {

    boolean existsByGroupAndUserIdAndInvitationStatusId(Group groupInvited, UUID userId, Integer invitationStatusInvitationStatusId);

    Optional<GroupInvitation> findByUserIdAndGroupAndInvitationStatus_InvitationStatusId(UUID userId, Group groupInvited, Integer invitationStatusInvitationStatusId);

    Optional<GroupInvitation> findByGroupInvitationIdAndGroupId(UUID groupInvitationId, UUID groupId);

    @EntityGraph(attributePaths = {"group"})
    Optional<GroupInvitation> findWithGroupByGroupInvitationIdAndGroupId(UUID groupInvitationId, UUID groupId);
}
