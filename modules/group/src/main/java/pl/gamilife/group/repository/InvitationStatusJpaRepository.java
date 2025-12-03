package pl.gamilife.group.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.group.model.InvitationStatus;

public interface InvitationStatusJpaRepository extends JpaRepository<InvitationStatus, Integer> {
}
