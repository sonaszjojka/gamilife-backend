package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupInvitation;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface GroupInvitationJpaRepository extends JpaRepository<GroupInvitation, UUID> {

    boolean existsByGroupInvitedAndUserIdAndInvitationStatus_InvitationStatusId(Group groupInvited, UUID userId, Integer invitationStatusInvitationStatusId);

    Optional<GroupInvitation> findByUserIdAndGroupInvitedAndInvitationStatus_InvitationStatusId(UUID userId, Group groupInvited, Integer invitationStatusInvitationStatusId);

    Optional<GroupInvitation> findByGroupInvitationIdAndGroupInvited_GroupId(UUID groupInvitationId, UUID groupId);
}
