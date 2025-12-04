package pl.gamilife.group.repository;

import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.group.model.GroupInvitation;

import java.util.Optional;
import java.util.UUID;

public interface GroupInvitationJpaRepository extends JpaRepository<GroupInvitation, UUID> {
    @EntityGraph(attributePaths = {"group"})
    Optional<GroupInvitation> findWithGroupByGroupInvitationIdAndGroupId(UUID groupInvitationId, UUID groupId);
}
