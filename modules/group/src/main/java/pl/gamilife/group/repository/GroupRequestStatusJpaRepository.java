package pl.gamilife.group.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.group.model.GroupRequestStatus;

public interface GroupRequestStatusJpaRepository extends JpaRepository<GroupRequestStatus, Integer> {
}
