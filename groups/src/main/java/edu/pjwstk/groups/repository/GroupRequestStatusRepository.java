package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.entity.GroupRequestStatus;

import java.util.Optional;

public interface GroupRequestStatusRepository {
    Optional<GroupRequestStatus> findById(Integer id);
}
