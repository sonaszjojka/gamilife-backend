package pl.gamilife.group.repository;

import pl.gamilife.group.model.Group;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface GroupJpaRepository extends JpaRepository<Group, UUID>, JpaSpecificationExecutor<Group> {

    @EntityGraph(attributePaths = {"groupMembers", "groupType"})
    Optional<Group> findWithGroupMembersByGroupId(UUID groupId);

    @EntityGraph(attributePaths = {"groupMembers",  "groupType"})
    List<Group> findWithGroupMembersByGroupIdIn(List<UUID> groupIds);

    @Override
    @EntityGraph(attributePaths = {"groupMembers", "groupType"})
    Page<Group> findAll(Specification<Group> spec, Pageable pageable);

    @EntityGraph(attributePaths = {"groupMembers", "groupType"})
    Optional<Group> findWithGroupMembersAndGroupTypeByGroupId(UUID groupId);
}
