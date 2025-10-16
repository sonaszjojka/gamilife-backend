package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.entity.GroupInvitation;

import java.util.Optional;
import java.util.UUID;

public interface GroupInvitationRepository {
    Optional<GroupInvitation> findById(UUID groupInvitationId);

    void deleteById(UUID groupInvitationId);
}
