package pl.gamilife.group.repository;

import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupRequest;
import pl.gamilife.group.model.GroupRequestStatus;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface GroupRequestJpaRepository extends JpaRepository<GroupRequest, UUID>, JpaSpecificationExecutor<GroupRequest> {
    boolean existsByGroupRequestedAndUserIdAndGroupRequestStatusId(Group group, UUID userId, Integer groupRequestStatus);

    Optional<GroupRequest> findByGroupRequestIdAndGroupRequested_GroupId(UUID groupRequestId, UUID groupId);

    boolean existsByGroupRequestedAndUserIdAndGroupRequestStatus(Group groupRequested, UUID userId, GroupRequestStatus groupRequestStatus);

    List<GroupRequest> findWithStatusByGroupRequestIdIn(Collection<UUID> groupRequestIds);
}
