package pl.gamilife.app;

import jakarta.persistence.EntityManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.transaction.annotation.Transactional;
import org.testcontainers.containers.PostgreSQLContainer;
import pl.gamilife.gamification.application.usecase.processuserregistered.ProcessUserRegisteredCommand;
import pl.gamilife.gamification.application.usecase.processuserregistered.ProcessUserRegisteredUseCase;
import pl.gamilife.user.persistence.User;
import pl.gamilife.user.persistence.jpa.JpaUserRepository;

@ActiveProfiles("test")
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@SpringBootTest(
        classes = StartUp.class,
        webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT
)
@Transactional
public abstract class BaseIntegrationTest {

    static final PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:17-alpine");

    static {
        postgres.start();
    }

    @Autowired
    private EntityManager entityManager;

    @Autowired
    private JpaUserRepository userRepository;

    @Autowired
    private ProcessUserRegisteredUseCase processUserRegisteredUseCase;

    @DynamicPropertySource
    static void configureProperties(DynamicPropertyRegistry registry) {
        registry.add("spring.datasource.url", postgres::getJdbcUrl);
        registry.add("spring.datasource.username", postgres::getUsername);
        registry.add("spring.datasource.password", postgres::getPassword);
    }

    protected User createUser() {
        return userRepository.save(User.builder()
                .username("testinvuser")
                .email("testinv@test.pl")
                .firstName("Inventory")
                .timezone("Europe/Warsaw")
                .isEmailVerified(true)
                .isProfilePublic(true)
                .sendBudgetReports(false)
                .build());
    }

    protected User createUserWithStats() {
        User user = createUser();

        processUserRegisteredUseCase.execute(new ProcessUserRegisteredCommand(user.getId()));

        return user;
    }

    protected void flushAndClear() {
        entityManager.flush();
        entityManager.clear();
    }
}