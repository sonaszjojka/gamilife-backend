package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.entity.InvitationStatus;

import java.util.Optional;

public interface InvitationStatusRepository {
    Optional<InvitationStatus> findById(Integer id);
}
