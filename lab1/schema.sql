-- Видаляємо стару базу даних, якщо вона існує, і створюємо нову
DROP DATABASE IF EXISTS department_resources;
CREATE DATABASE department_resources CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
USE department_resources;

-- Таблиця для авторів
CREATE TABLE authors (
    id INT AUTO_INCREMENT PRIMARY KEY,
    full_name VARCHAR(255) NOT NULL,
    contact_info VARCHAR(255)
);

-- Таблиця для типів ресурсів (наприклад, "Книга", "Стаття", "Програмне забезпечення")
CREATE TABLE resource_types (
    id INT AUTO_INCREMENT PRIMARY KEY,
    type_name VARCHAR(100) NOT NULL UNIQUE
);

-- Основна таблиця для інформаційних ресурсів
CREATE TABLE resources (
    id INT AUTO_INCREMENT PRIMARY KEY,
    resource_name VARCHAR(255) NOT NULL,
    author_id INT,
    resource_type_id INT,
    abstract_text TEXT,
    purpose TEXT,
    release_date DATE,
    term_of_use INT, -- Термін використання, наприклад, у днях
    terms_of_use_desc TEXT, -- Текстовий опис умов
    FOREIGN KEY (author_id) REFERENCES authors(id),
    FOREIGN KEY (resource_type_id) REFERENCES resource_types(id)
);

-- Таблиця для веб-адрес (один ресурс може мати кілька)
CREATE TABLE web_addresses (
    id INT AUTO_INCREMENT PRIMARY KEY,
    resource_id INT,
    url VARCHAR(255) NOT NULL,
    FOREIGN KEY (resource_id) REFERENCES resources(id) ON DELETE CASCADE
);

-- Таблиця для користувачів системи
CREATE TABLE users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    user_name VARCHAR(100) NOT NULL UNIQUE,
    registration_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Таблиця для статистики використання ресурсів
CREATE TABLE usage_stats (
    id INT AUTO_INCREMENT PRIMARY KEY,
    user_id INT,
    resource_id INT,
    access_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    notes TEXT,
    FOREIGN KEY (user_id) REFERENCES users(id),
    FOREIGN KEY (resource_id) REFERENCES resources(id)
);

-- Додамо трохи початкових даних для тестування
INSERT INTO authors (full_name, contact_info) VALUES ('Верес М.М.', 'veres@univ.net'), ('Галкін О.В.', 'halkin@univ.net');
INSERT INTO resource_types (type_name) VALUES ('Навчальний посібник'), ('Стаття'), ('Презентація');
