package pl.gamilife.group.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupRequest;
import pl.gamilife.group.model.GroupRequestStatus;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface GroupRequestJpaRepository extends JpaRepository<GroupRequest, UUID>, JpaSpecificationExecutor<GroupRequest> {
    boolean existsByGroupAndUserIdAndStatusId(Group group, UUID userId, Integer groupRequestStatus);

    Optional<GroupRequest> findByIdAndGroupId(UUID groupRequestId, UUID groupId);

    boolean existsByGroupAndUserIdAndStatus(Group groupRequested, UUID userId, GroupRequestStatus groupRequestStatus);

    List<GroupRequest> findWithStatusByIdIn(Collection<UUID> groupRequestIds);
}
