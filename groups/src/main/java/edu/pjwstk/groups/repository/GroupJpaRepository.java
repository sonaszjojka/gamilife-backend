package edu.pjwstk.groups.repository;

import edu.pjwstk.groups.model.Group;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.Optional;
import java.util.UUID;

public interface GroupJpaRepository extends JpaRepository<Group, UUID>, JpaSpecificationExecutor<Group> {
    @EntityGraph(attributePaths = {"groupMembers"})
    Optional<Group> findWithGroupMembersByGroupId(UUID groupId);
}
