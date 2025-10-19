package edu.pjwstk.groups.repository.jpa;

import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupInvitation;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface GroupInvitationRepositoryJpa extends JpaRepository<GroupInvitation, UUID> {

    boolean existsByGroupInvitedAndUserIdAndInvitationStatus_InvitationStatusId(Group groupInvited, UUID userId, Integer invitationStatusInvitationStatusId);

    Optional<GroupInvitation> findByUserIdAndGroupInvitedAndInvitationStatus_InvitationStatusId(UUID userId, Group groupInvited, Integer invitationStatusInvitationStatusId);
}
