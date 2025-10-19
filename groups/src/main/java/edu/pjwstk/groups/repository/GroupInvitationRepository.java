package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupInvitation;
import edu.pjwstk.groups.shared.InvitationStatusEnum;

import java.util.Optional;
import java.util.UUID;

public interface GroupInvitationRepository {
    Optional<GroupInvitation> findById(UUID groupInvitationId);

    void deleteById(UUID groupInvitationId);

    GroupInvitation save(GroupInvitation groupInvitation);

    // not sure about name of this method
    boolean existsByGroupInvitedAndUserIdAndInvitationStatus(Group group, UUID userId, InvitationStatusEnum invitationStatusEnum);

    Optional<GroupInvitation> findByUserIdAndGroupInvitedAndInvitationStatus_InvitationStatusId(UUID uuid, Group group, InvitationStatusEnum invitationStatusEnum);
}
