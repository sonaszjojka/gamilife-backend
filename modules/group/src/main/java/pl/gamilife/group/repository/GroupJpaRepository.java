package pl.gamilife.group.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import pl.gamilife.group.model.Group;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface GroupJpaRepository extends JpaRepository<Group, UUID>, JpaSpecificationExecutor<Group> {

    @EntityGraph(attributePaths = {"activeMembers", "type"})
    Optional<Group> findWithActiveMembersById(UUID groupId);

    @EntityGraph(attributePaths = {"activeMembers", "type"})
    List<Group> findWithActiveMembersByIdIn(List<UUID> groupIds);

    @Override
    @EntityGraph(attributePaths = {"activeMembers", "type"})
    Page<Group> findAll(Specification<Group> spec, Pageable pageable);

    @EntityGraph(attributePaths = {"activeMembers", "type"})
    Optional<Group> findWithActiveMembersAndGroupTypeById(UUID groupId);
}
