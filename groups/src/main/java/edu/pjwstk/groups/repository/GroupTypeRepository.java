package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.domain.GroupType;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;

import java.util.Optional;

public interface GroupTypeRepository {
    Optional<GroupType> findById(Integer groupTypeId);
}
