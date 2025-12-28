package pl.gamilife.group.repository;

import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupMember;

import java.time.Instant;
import java.util.Optional;
import java.util.UUID;

public interface GroupMemberJpaRepository extends JpaRepository<GroupMember, UUID> {

    @EntityGraph(attributePaths = {"group"})
    Optional<GroupMember> findByUserIdAndGroup(UUID userId, Group memberGroup);

    @EntityGraph(attributePaths = {"group"})
    Optional<GroupMember> findByIdAndGroupId(UUID groupMemberId, UUID groupId);

    @EntityGraph(attributePaths = {"group"})
    Optional<GroupMember> findWithGroupByIdAndGroupId(UUID groupMemberId, UUID groupId);

    @Query("""
                SELECT gm
                FROM GroupMember gm
                WHERE gm.userId = :userId
                  AND gm.group = :group
                  AND gm.leftAt IS NULL
            """)
    @EntityGraph(attributePaths = {"group"})
    Optional<GroupMember> findActiveMember(UUID userId, Group group);

    boolean existsByUserIdAndGroupAndLeftAt(UUID userId, Group group, Instant leftAt);
}
