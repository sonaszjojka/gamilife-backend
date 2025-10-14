package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.entity.GroupType;

import java.util.Optional;

public interface GroupTypeRepository {
    Optional<GroupType> findById(Integer groupTypeId);
}
