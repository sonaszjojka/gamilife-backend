package pl.gamilife.group.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.group.model.GroupType;

public interface GroupTypeJpaRepository extends JpaRepository<GroupType, Integer> {
}
