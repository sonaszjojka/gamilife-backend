package pl.gamilife.group.repository;

import jakarta.validation.constraints.NotNull;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.group.model.GroupInvitation;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface GroupInvitationJpaRepository extends JpaRepository<GroupInvitation, UUID> {
    @EntityGraph(attributePaths = {"group"})
    Optional<GroupInvitation> findWithGroupByIdAndGroupId(UUID groupInvitationId, UUID groupId);

    List<GroupInvitation> findByGroupIdAndUserId(UUID groupId, UUID userId);

    void deleteAllByGroupId(@NotNull UUID groupId);
}
