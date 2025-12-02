package pl.gamilife.group.repository;

import pl.gamilife.group.model.GroupRequestStatus;
import org.springframework.data.jpa.repository.JpaRepository;

public interface GroupRequestStatusJpaRepository extends JpaRepository<GroupRequestStatus, Integer> {
}
