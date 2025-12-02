package pl.gamilife.group.repository;

import pl.gamilife.group.model.InvitationStatus;
import org.springframework.data.jpa.repository.JpaRepository;

public interface InvitationStatusJpaRepository extends JpaRepository<InvitationStatus, Integer> {
}
